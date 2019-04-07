#include <err.h>
#include <stdio.h>
#include <stdlib.h>

struct val {
  enum tag {
    CLOSURE_T, INT_T
  } tag;
  union {
    struct {
      struct val *(*code)(struct val **, struct val *);
      struct val **env;
    };
    int val;
  } c;
};

struct val *mk_closure(struct val *(*code)(struct val **, struct val*), struct val **env) {
  struct val *ret = malloc(sizeof(struct val));
  if (!ret) {
    err(1, "could not allocate storage for closure");
  }
  ret->tag = CLOSURE_T;
  ret->c.code = code;
  ret->c.env = env;
  return ret;
}

struct val *mk_int(int val) {
  struct val *ret = malloc(sizeof(struct val));
  if (!ret) {
    err(1, "could not allocate storage for closure");
  }
  ret->tag = INT_T;
  ret->c.val = val;
  return ret;
}

int dump (struct val *val) {
  switch (val->tag) {
    case CLOSURE_T:
      return printf("<closure>\n");
    case INT_T:
      return printf("%d\n", val->c.val);
  };
}
