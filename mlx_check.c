/*
 * mlx_check
 *
 * This tool looks for mlx_ifc_* structs definitions and dumps their
 * layout to a JSON file. It also does some checks.
 *
 * Copyright (C) 2024 Aurelien Aptel <aaptel@nvidia.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdarg.h>
#include <assert.h>

#include "parse.h"
#include "scope.h"
#include "symbol.h"

#include "options.h"

struct mlx_context
{
        bool dump_complete;
	bool dump_json;
	FILE *json;

	bool first_member;
	bool first_struct;
};

static void examine_symbol(struct mlx_context *ctx, struct symbol *sym, int n, int parent_off);

static inline bool is_compound(struct symbol *sym)
{
        return sym->type == SYM_STRUCT || sym->type == SYM_UNION;
}

static const char *sym_type_name(const struct symbol *sym)
{
        enum type type = sym->type;
	static const char *type_name[] = {
		[SYM_UNINITIALIZED] = "UNINITIALIZED",
		[SYM_PREPROCESSOR] = "PREPROCESSOR",
		[SYM_BASETYPE] = "BASETYPE",
		[SYM_NODE] = "NODE",
		[SYM_PTR] = "PTR",
		[SYM_FN] = "FN",
		[SYM_ARRAY] = "ARRAY",
		[SYM_STRUCT] = "STRUCT",
		[SYM_UNION] = "UNION",
		[SYM_ENUM] = "ENUM",
		[SYM_TYPEOF] = "TYPEOF",
		[SYM_BITFIELD] = "BITFIELD",
		[SYM_LABEL] = "LABEL",
		[SYM_RESTRICT] = "RESTRICT",
		[SYM_FOULED] = "FOULED",
		[SYM_KEYWORD] = "KEYWORD",
		[SYM_BAD] = "BAD",
	};
	return type_name[type] ?: "UNKNOWN_TYPE";
}

static void dump_json_start(struct mlx_context *ctx)
{
	if (!ctx->dump_json)
		return;

	fprintf(ctx->json, "{\n");
	ctx->first_struct = 1;
}

static void dump_json_end(struct mlx_context *ctx)
{
	if (!ctx->dump_json)
		return;

	fprintf(ctx->json, "\n}\n");
}

static void dump_json_members(struct mlx_context *ctx, struct symbol_list *list, int parent_off)
{
	struct symbol *s;

	FOR_EACH_PTR(list, s) {
		if (s->ident) {
			fprintf(ctx->json, "%s\t\t\t\"%s\" : { \"offset\": %d,"
				"  \"size\" : %d}",
				ctx->first_member ? "" : ",\n",
				show_ident(s->ident),
				parent_off + (int)s->offset,
				s->bit_size/8);
			ctx->first_member = 0;

		} else {
			// otherwise its an unnamed union/struct, recurse until
			// we find something with a name
			struct symbol *base;

			assert(s->type == SYM_NODE);
			base = s->ctype.base_type;

			if (is_compound(base)) {
				dump_json_members(ctx, base->symbol_list, parent_off + s->offset);
			} else {
				die("unknown unamed sym type %s\n", sym_type_name(base));
			}
		}
	} END_FOR_EACH_PTR(s);
}

static void dump_json_struct(struct mlx_context *ctx, struct symbol *cstruct)
{
	if (!ctx->dump_json)
		return;

	const char *name = show_ident(cstruct->ident);
	const char *start = name + strlen("mlx5_ifc_");
	const char *end = strstr(start, "_bits");
	int len;
	int i;
	int size = cstruct->bit_size / 8;
	if (end) {
		len = end - start;
	} else {
		len = strlen(start);
	}

	fprintf(ctx->json,
		"%s"
		"\t\"%.*s\" : {\n"
		"\t\t\"size\" : %d ,\n"
		"\t\t\"fields\" : {\n",
		ctx->first_struct ? "" : ",\n",
		len, start,
		size);

	ctx->first_member = 1;
	dump_json_members(ctx, cstruct->symbol_list, 0);

	fprintf(ctx->json, "\n} }");
	ctx->first_struct = 0;
}

void print(struct mlx_context *ctx, int n, const char *format, ...) {
    va_list args;

    if (!ctx->dump_complete)
            return;

    while (n) {
	    printf("  ");
	    n--;
    }

    va_start(args, format);
    vprintf(format, args);
    va_end(args);
}

static int reserved_field_at_offset(struct symbol *sym)
{
	const char *search = "reserved_at_";
	const char *p = strstr(show_ident(sym->ident), search);
	char *end;
	unsigned long r;

	if (!p)
		return -1;

	r = strtoul(p + strlen(search), &end, 16);
	if (*p && !*end)
		return r;

	warning(sym->pos, "cannot parse hex for reserved_at field <%s>\n", show_ident(sym->ident));

	return -1;
}

static inline void examine_members(struct mlx_context *ctx, struct symbol_list *list, int n, int parent_off)
{
	struct symbol *sym;
	int off;

	FOR_EACH_PTR(list, sym) {
		int abs_off = parent_off + sym->offset;

                print(ctx, n, "[0x%x ; %d", sym->offset, sym->bit_size/8);
		if (n > 1)
			print(ctx, 0, "; abs 0x%x", abs_off);
		print(ctx, 0, "]\n");

		off = reserved_field_at_offset(sym);
		if (off > 0 && off != sym->offset) {
			warning(sym->pos, "misnamed field <%s> is actually at offset 0x%lx", show_ident(sym->ident), sym->offset);
		}

		examine_symbol(ctx, sym, n, abs_off);
	} END_FOR_EACH_PTR(sym);
}

static inline bool is_ifc_struct(struct symbol *sym)
{
	return is_compound(sym) && sym->ident && strstr(show_ident(sym->ident), "mlx5_ifc_");
}

static void examine_symbol(struct mlx_context *ctx, struct symbol *sym, int n, int parent_off)
{
	struct symbol *base = sym;

	if (!sym)
		return;
	if (sym->ident && sym->ident->reserved)
		return;
	if (sym->type == SYM_KEYWORD || sym->type == SYM_PREPROCESSOR)
		return;

        if (n == 0 && is_ifc_struct(sym)) {
		// only dump top-level ifc structs
                if (dump_ifc)
			ctx->dump_complete = 1;
		dump_json_struct(ctx, sym);
        }

	base = sym->ctype.base_type;

        print(ctx, n, "%s '%s' size=%d\n", sym_type_name(sym), show_ident(sym->ident), sym->bit_size/8);
	if (sym->visited)
		return;
	if (sym->ident && !sym->visited) {
		// dont recurse infinitely in self-referencing type cycles
		sym->visited = 1;
	}

	switch (sym->type) {
	case SYM_NODE:
		examine_symbol(ctx, base, n+1, parent_off);
		break;
	case SYM_STRUCT:
		if (sym->ident && !sym->symbol_list) {
			sparse_error(sym->pos, "missing struct %s definition", show_ident(sym->ident));
		}
		examine_members(ctx, sym->symbol_list, n+1, parent_off);
		break;
	case SYM_UNION:
		examine_members(ctx, sym->symbol_list, n+1, parent_off);
		break;
	case SYM_ENUM:
	case SYM_PTR:
	case SYM_TYPEOF:
	case SYM_BITFIELD:
	case SYM_FN:
                examine_symbol(ctx, sym->ctype.base_type, n+1, parent_off);
                break;
	case SYM_ARRAY:
		examine_symbol(ctx, sym->ctype.base_type, n+1, parent_off);
		break;
	case SYM_BASETYPE:
        case SYM_RESTRICT:
		break;

	default:
		die("unknown symbol %s namespace:%d type:%d\n", show_ident(sym->ident),
		    sym->namespace, sym->type);
	}
	return;
}

static void examine_namespace(struct mlx_context *ctx, struct symbol *sym)
{
	if (sym->visited)
		return;
	if (sym->ident && sym->ident->reserved)
		return;

	switch(sym->namespace) {
	case NS_KEYWORD:
	case NS_PREPROCESSOR:
	case NS_LABEL:
	case NS_MACRO:
	case NS_UNDEF:
                return;

	case NS_TYPEDEF:
                //printf("== NS_TYPEDEF ==\n");
                examine_symbol(ctx, sym, 0, 0);
                break;
	case NS_SYMBOL:
                //printf("== NS_SYMBOL ==\n");
                examine_symbol(ctx, sym, 0, 0);
                break;
	case NS_STRUCT:
                //printf("== NS_STRUCT ==\n");
                examine_symbol(ctx, sym, 0, 0);
		break;
	default:
		die("unknown namespace %d symbol:%s type:%d\n", sym->namespace,
		    show_ident(sym->ident), sym->type);
	}
}

static inline void examine_symbol_list(struct mlx_context *ctx, struct symbol_list *list)
{
	struct symbol *sym;
	if (!list)
		return;
	FOR_EACH_PTR(list, sym) {
		struct symbol *x = sym;

		x = examine_symbol_type(sym);
		if (!x) {
			warning(sym->pos, "unknown symbol type error");
			continue;
		}
		if (x != sym) {
			warning(sym->pos, "symbol type address changed");
		}
		sym = x;
		examine_namespace(ctx, sym);

	} END_FOR_EACH_PTR(sym);
}

int main(int argc, char **argv)
{
	struct string_list *filelist = NULL;
	struct mlx_context ctx = {0};
	char *file;
	sparse_initialize(argc, argv, &filelist);

	if (dump_ifc_size_out) {
		ctx.json = fopen(dump_ifc_size_out, "w+");
		if (!ctx.json)
			die("cannot open %s", dump_ifc_size_out);
		ctx.dump_json = 1;
	}

	dump_json_start(&ctx);

	FOR_EACH_PTR(filelist, file) {
		sparse(file);
		examine_symbol_list(&ctx, file_scope->symbols);

	} END_FOR_EACH_PTR(file);

	dump_json_end(&ctx);

	return 0;
}
