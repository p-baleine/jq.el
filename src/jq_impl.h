#ifndef __JQ_IMPL_H_
#define __JQ_IMPL_H_

#include <emacs-module.h>

static emacs_value jq_impl_init(emacs_env*, ptrdiff_t, emacs_value*, void*) noexcept;
static emacs_value jq_impl_next(emacs_env*, ptrdiff_t, emacs_value*, void*) noexcept;

#endif // __JQ_IMPL_H_
