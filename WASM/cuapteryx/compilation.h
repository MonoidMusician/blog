#ifndef __has_feature
  #define __has_feature(x) 0  // Compatibility with non-clang compilers.
#endif
#ifndef __has_extension
  #define __has_extension __has_feature // Compatibility with pre-3.0 compilers.
#endif

// https://clang.llvm.org/docs/AttributeReference.html#always-inline-force-inline
#if __has_extension(cxx_attributes)
#define PLEASE_INLINE_FOLLOWING_STATEMENT [[clang::always_inline]]
#define PLEASE_INLINABLE
#else
#define PLEASE_INLINE_FOLLOWING_STATEMENT
#define PLEASE_INLINABLE __attribute__((always_inline))
#endif

#if __has_builtin(__builtin_expect)
#define RARELY(c) (__builtin_expect((c), 0))
#else
#define RARELY(c) (c)
#endif

#if __has_extension(cxx_attributes)
#define TAILCALL [[clang::musttail]]
#else
#define TAILCALL __attribute__((musttail))
#endif

#if __has_attribute(import_name)
// Set it up as a WASM import
#define OPTIONAL_IMPORT_OR_VOID(name, ...) \
  __attribute__((import_name(#name))) \
  extern void name(__VA_ARGS__);
#else
// Define an empty stub
#define OPTIONAL_IMPORT_OR_VOID(name, ...) void name(__VA_ARGS__) {}
#endif
