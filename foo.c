typedef unsigned char u8;


struct aaa;

struct mlx5_ifc_abc {};

struct mlx5_ifc_blah {
	u8 foo[1];
	//struct xxx a;
	struct xxx *b;
};




/*
union foo {
  int bar;
};

struct mlx5_ifc_foo {
  u8 a[3];
  union {
    struct {
      u8 b1[1];
      u8 b2[1];
    } s1;
    struct {
      u8 c1[1];
      u8 c2[2];
    } s2;
  };
};

struct mlx5_ifc_bar {
  u8 d;
  struct mlx5_ifc_foo x;
};

struct mlx5_ifc_flex {
  u8 x;
  union {
    u8 u1[5][];
    u8 u2[6][];
  };
};


void func(void) {
	struct mlx5_ifc_foo aaa;
	aaa.a[0] = 1;
	aaa.s1.b2[0] = 2;
}

*/
