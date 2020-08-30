#include <emacs-module.h>
#include "utils.h"

void bind_function(emacs_env* env, const char* name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);
  emacs_value args[] = { Qsym, Sfun };
  env->funcall(env, Qfset, 2, args);
}

void provide(emacs_env* env, const char* feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = { Qfeat };
  env->funcall(env, Qprovide, 1, args);
}
