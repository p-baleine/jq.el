#include <assert.h>
#include <emacs-module.h>
#include <sstream>
#include <string>

#include <iostream>

extern "C" {
#include <jv.h>
#include <jq.h>
}

#include "utils.h"
#include "jq_impl.h"

int plugin_is_GPL_compatible;

int emacs_module_init(struct emacs_runtime* ert) {
  // Verify the compatibility of the Emacs executable.
  if (static_cast<long unsigned int>(ert->size) < sizeof(*ert))
    return 1;

  emacs_env *env = ert->get_environment (ert);

  // Verify the compatibility of the module API.
  if (static_cast<long unsigned int>(env->size) < sizeof(*env))
    return 2;

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function(                                 \
    env, lsym,                                   \
    env->make_function(env, amin, amax, csym, doc, data))

  DEFUN("jq-impl-init", jq_impl_init, 4, 4, NULL, NULL);
  DEFUN("jq-impl-next", jq_impl_next, 1, 1, NULL, NULL);

  provide(env, "jq-impl");

  return 0;
}

static void jq_impl_teardown(void* jq) noexcept {
  jq_state* jq_ = reinterpret_cast<jq_state*>(jq);
  jq_teardown(&jq_);
}

static emacs_value jq_impl_init(
  emacs_env* env, ptrdiff_t nargs, emacs_value* args, void* data) noexcept {
  auto input_size = env->extract_integer(env, args[1]) + 1;
  auto program_size = env->extract_integer(env, args[3]) + 1;
  char input[input_size];
  char program[program_size];
  env->copy_string_contents(env, args[0], input, &input_size);
  env->copy_string_contents(env, args[2], program, &program_size);

  jq_state* jq = jq_init();

  int compiled = jq_compile_args(jq, program, jv_object());

  std::string ret;

  if (!compiled) {
    // TODO: エラーハンドリング、どうやるの？？
    std::cerr << "jq_compile_args failed." << std::endl;
  } else {
    jv_parser* parser = jv_parser_new(0);
    jv_parser_set_buf(parser, input, input_size, 0);
    jv value = jv_parser_next(parser);

    if (jv_is_valid(value)) {
      jq_start(jq, value, 0);
      return env->make_user_ptr(env, jq_impl_teardown, jq);
    } else if (jv_invalid_has_msg(jv_copy(value))) {
      std::cerr << "jq_parse_next parse failed." << std::endl;
    } else {
      std::cerr << "jq_impl system error." << std::endl;
    }
  }
}

static emacs_value jq_impl_next(
  emacs_env* env, ptrdiff_t nargs, emacs_value* args, void* data) noexcept {
  emacs_value nil = env->intern(env, "nil");
  jq_state* jq = reinterpret_cast<jq_state*>(env->get_user_ptr(env, args[0]));
  jv result = jq_next(jq);

  if (!jv_is_valid(result)) {
    jv_free(result);
    return nil;
  }

  jv dumped = jv_dump_string(jv_copy(result), 0);
  std::string dumped_str = jv_string_value(dumped);

  jv_free(dumped);
  jv_free(result);

  return env->make_string(env, dumped_str.c_str(), dumped_str.size());
}