#include <emacs-module.h>

#ifndef JQEL_IMPL_UTILS_H_
#define JQEL_IMPL_UTILS_H_

void bind_function(emacs_env*, const char*, emacs_value);
void provide(emacs_env*, const char*);

#endif // JQEL_IMPL_UTILS_H_
