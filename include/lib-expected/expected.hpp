#ifndef LIB_EXPECTED_EXPECTED_HPP_
#define LIB_EXPECTED_EXPECTED_HPP_

#include <cassert>
#include <functional>
#include <initializer_list>
#include <stdexcept>
#include <string>
#include <type_traits>

namespace libExpected {

namespace Detail {
    template <typename TOtherError>
    struct ExpectedError {
        using Type = typename std::decay<TOtherError>::type;
    };

    template <std::size_t TSize>
    struct ExpectedError<const char (&)[TSize]> {
        using Type = std::string;
    };

    template <std::size_t TSize>
    struct ExpectedError<const wchar_t (&)[TSize]> {
        using Type = std::wstring;
    };
} // namespace Detail

class InPlaceT final {
public:
    InPlaceT() = delete;

    enum class Construct { Token };

    explicit constexpr InPlaceT(Construct) {}
};

static constexpr InPlaceT InPlace(InPlaceT::Construct::Token);

template <typename T>
class Unexpected final {
public:
    using ValueType = T;

    Unexpected() = delete;

    explicit Unexpected(T error)
        : mValue(std::move(error)) {}

    const ValueType& value() const& noexcept(false) { return mValue; }

    ValueType& value() & noexcept(false) { return mValue; }

    ValueType&& value() && noexcept(false) { return std::move(mValue); }

private:
    ValueType mValue;
};

template <class TError>
constexpr bool operator==(const Unexpected<TError>& lhs, const Unexpected<TError>& rhs) {
    return lhs.value() == rhs.value();
}

template <class TError>
constexpr bool operator!=(const Unexpected<TError>& lhs, const Unexpected<TError>& rhs) {
    return lhs.value() != rhs.value();
}

template <class TError>
constexpr bool operator<(const Unexpected<TError>& lhs, const Unexpected<TError>& rhs) {
    return lhs.value() < rhs.value();
}

template <class TError>
constexpr bool operator>(const Unexpected<TError>& lhs, const Unexpected<TError>& rhs) {
    return lhs.value() > rhs.value();
}

template <class TError>
constexpr bool operator<=(const Unexpected<TError>& lhs, const Unexpected<TError>& rhs) {
    return lhs.value() <= rhs.value();
}

template <class TError>
constexpr bool operator>=(const Unexpected<TError>& lhs, const Unexpected<TError>& rhs) {
    return lhs.value() >= rhs.value();
}

template <typename T>
Unexpected<typename Detail::ExpectedError<T>::Type> makeUnexpected(T&& error) {
    return Unexpected<typename Detail::ExpectedError<T>::Type>(std::forward<T>(error));
}

class BadExpectedAccess : public std::exception {
public:
    BadExpectedAccess() noexcept = default;

    virtual const char* what() const noexcept override { return "Bad Expected access"; }
};

namespace swapDetail {
    using std::swap;

    template <typename T>
    void adlSwap(T& t, T& u) noexcept(noexcept(swap(t, u))) {
        swap(t, u);
    }

} // namespace swapDetail

namespace detail {
    template <bool TTest, typename TTrue, typename TFalse>
    using Conditional = typename std::conditional<TTest, TTrue, TFalse>::type;

    template <typename T>
    using IsReference = std::is_reference<T>;

    template <typename T>
    using RemoveReference = typename std::remove_reference<T>::type;

    template <typename T>
    using ReferenceWrapper = std::reference_wrapper<T>;

    template <typename T>
    using ReferenceStorage =
        Conditional<IsReference<T>::value, ReferenceWrapper<RemoveReference<T>>, RemoveReference<T>>;

    template <bool TTest, typename TType = void>
    using EnableIf = typename std::enable_if<TTest, TType>::type;

    class Copyable {
    public:
        Copyable() = default;
        Copyable(const Copyable&) = default;
        Copyable& operator=(const Copyable&) = default;
    };

    class Movable {
    public:
        Movable() = default;
        Movable(Movable&&) = default;
        Movable& operator=(Movable&&) = default;
    };

    class Noncopyable {
    public:
        Noncopyable() = default;
        Noncopyable(const Noncopyable&) = delete;
        Noncopyable& operator=(const Noncopyable&) = delete;
    };

    class Nonmovable {
    public:
        Nonmovable() = default;
        Nonmovable(Nonmovable&&) = delete;
        Nonmovable& operator=(Nonmovable&&) = delete;
    };

} // namespace detail

template <typename T, typename TError = std::string>
class Expected final {
public:
    static_assert(!std::is_rvalue_reference<T>::value, "Expected cannot be used with r-value references");
    static_assert(!std::is_same<T, InPlaceT>::value, "Expected cannot be used with InPlaceT");
    static_assert(!std::is_same<T, Unexpected<T>>::value, "Expected cannot be used with Unexpected");

    using ValueType = detail::ReferenceStorage<T>;
    using ErrorType = TError;
    using value_type = ValueType; // std traits
    using error_type = ErrorType; // std traits
    using TRaw = typename std::remove_const<typename std::remove_reference<T>::type>::type;
    using TPtr = TRaw*;
    using TConstPtr = TRaw const*;
    using TRef = T&;
    using TConstRef = T const&;

private:
    template <typename TOther>
    static constexpr bool IsConstructibleOrConvertibleFrom() {
        return std::is_constructible<ValueType, Expected<TOther>&>::value ||
               std::is_constructible<ValueType, const Expected<TOther>&>::value ||
               std::is_constructible<ValueType, Expected<TOther>&&>::value ||
               std::is_constructible<ValueType, const Expected<TOther>&&>::value ||
               std::is_convertible<Expected<TOther>&, ValueType>::value ||
               std::is_convertible<const Expected<TOther>&, ValueType>::value ||
               std::is_convertible<Expected<TOther>&&, ValueType>::value ||
               std::is_convertible<const Expected<TOther>&&, ValueType>::value;
    }

    template <typename TOther>
    static constexpr bool IsAssignableFrom() {
        return std::is_assignable<ValueType&, Expected<TOther>&>::value ||
               std::is_assignable<ValueType&, const Expected<TOther>&>::value ||
               std::is_assignable<ValueType&, Expected<TOther>&&>::value ||
               std::is_assignable<ValueType&, const Expected<TOther>&&>::value;
    }

public:
    /// Copy constructor
    Expected(const Expected& other) noexcept(
        std::is_nothrow_constructible<ValueType>::value&& std::is_nothrow_constructible<ErrorType>::value) {
        static_assert(std::is_copy_constructible<ValueType>::value &&
                          std::is_copy_constructible<ErrorType>::value,
                      "The underlying type of Expected must be copy-constructible");
        if (other.mHasValue) {
            construct(other.mValue);
        } else {
            constructError(other.mError);
        }
    }

    /// Move constructor
    Expected(Expected&& other) noexcept(
        std::is_nothrow_constructible<ValueType>::value&& std::is_nothrow_constructible<ErrorType>::value) {
        static_assert(std::is_move_constructible<ValueType>::value &&
                          std::is_move_constructible<ErrorType>::value,
                      "The underlying type of Expected must be move-constructible");
        if (other.mHasValue) {
            construct(std::move(other.mValue));
        } else {
            constructError(std::move(other.mError));
        }
    }

    /// Converting copy constructor
    ///
    /// Only available if ValueType is copy-constructible
    /// \note Conditionally explicit
    template <typename TOther,
              typename TOtherError,
              detail::EnableIf<
                  (!std::is_same<ValueType, TOther>::value || !std::is_same<ErrorType, TOtherError>::value) &&
                      std::is_constructible<ValueType, const typename Expected<TOther>::ValueType&>::value &&
                      std::is_constructible<ErrorType, TOtherError&>::value &&
                      std::is_convertible<const typename Expected<TOther>::ValueType&, ValueType>::value &&
                      !IsConstructibleOrConvertibleFrom<TOther>(),
                  bool> = true>
    Expected(const Expected<TOther, TOtherError>& other) {
        if (other.mHasValue) {
            construct(other.mValue);
        } else {
            constructError(other.mError);
        }
    }

    template <typename TOther,
              typename TOtherError,
              detail::EnableIf<
                  (!std::is_same<ValueType, TOther>::value || !std::is_same<ErrorType, TOtherError>::value) &&
                      std::is_constructible<ValueType, const typename Expected<TOther>::ValueType&>::value &&
                      std::is_constructible<ErrorType, TOtherError&>::value &&
                      !std::is_convertible<const typename Expected<TOther>::ValueType&, ValueType>::value &&
                      !IsConstructibleOrConvertibleFrom<TOther>(),
                  bool> = false>
    explicit Expected(const Expected<TOther, TOtherError>& other) {
        if (other.mHasValue) {
            construct(other.mValue);
        } else {
            constructError(other.mError);
        }
    }

    /// Converting move constructor
    ///
    /// Only available if ValueType is move-constructible
    /// \note Conditionally explicit
    template <typename TOther,
              typename TOtherError,
              detail::EnableIf<
                  (!std::is_same<ValueType, TOther>::value || !std::is_same<ErrorType, TOtherError>::value) &&
                      std::is_constructible<ValueType, const typename Expected<TOther>::ValueType&&>::value &&
                      std::is_constructible<ErrorType, TOtherError&&>::value &&
                      std::is_convertible<const typename Expected<TOther>::ValueType&&, ValueType>::value &&
                      !IsConstructibleOrConvertibleFrom<TOther>(),
                  bool> = true>
    Expected(Expected<TOther, TOtherError>&& other) {
        if (other.mHasValue) {
            construct(std::move(other.mValue));
        } else {
            constructError(std::move(other.mError));
        }
    }

    template <typename TOther,
              typename TOtherError,
              detail::EnableIf<
                  (!std::is_same<ValueType, TOther>::value || !std::is_same<ErrorType, TOtherError>::value) &&
                      std::is_constructible<ValueType, const typename Expected<TOther>::ValueType&&>::value &&
                      std::is_constructible<ErrorType, TOtherError&&>::value &&
                      !std::is_convertible<const typename Expected<TOther>::ValueType&&, ValueType>::value &&
                      !IsConstructibleOrConvertibleFrom<TOther>(),
                  bool> = false>
    explicit Expected(Expected<TOther, TOtherError>&& other) {
        if (other.mHasValue) {
            construct(std::move(other.mValue));
        } else {
            constructError(std::move(other.mError));
        }
    }

    /// In place constructor
    template <typename... TArgs,
              typename = detail::EnableIf<std::is_constructible<ValueType, TArgs&&...>::value>>
    explicit Expected(InPlaceT,
                      TArgs&&... args) noexcept(std::is_nothrow_constructible<ValueType, TArgs...>::value) {
        construct(std::forward<TArgs>(args)...);
    }

    template <typename TOther,
              typename... TArgs,
              typename = detail::EnableIf<
                  std::is_constructible<ValueType, std::initializer_list<TOther>&, TArgs&&...>::value>>
    explicit Expected(InPlaceT,
                      std::initializer_list<TOther> list,
                      TArgs&&... args) noexcept(std::is_nothrow_constructible<ValueType, TArgs...>::value) {
        construct(list, std::forward<TArgs>(args)...);
    }

    /// Constructor
    template <
        typename TOther = ValueType,
        detail::EnableIf<std::is_constructible<ValueType, TOther&&>::value &&
                             std::is_convertible<typename Expected<TOther>::ValueType&&, ValueType>::value,
                         bool> = true>
    Expected(TOther&& value) noexcept(std::is_nothrow_constructible<ValueType, TOther&&>::value) {
        construct(std::forward<TOther>(value));
    }

    template <
        typename TOther = ValueType,
        detail::EnableIf<std::is_constructible<ValueType, TOther&&>::value &&
                             !std::is_convertible<typename Expected<TOther>::ValueType&&, ValueType>::value,
                         bool> = false>
    explicit Expected(TOther&& value) noexcept(std::is_nothrow_constructible<ValueType, TOther&&>::value) {
        construct(std::forward<TOther>(value));
    }

    template <typename TOtherError,
              detail::EnableIf<std::is_constructible<ErrorType, TOtherError&&>::value &&
                                   std::is_convertible<TOtherError&&, TError>::value,
                               bool> = true>
    Expected(Unexpected<TOtherError> unexpected) noexcept(
        std::is_nothrow_constructible<ErrorType, TOtherError&&>::value) {
        new (reinterpret_cast<void*>(&mError)) TError(std::forward<TOtherError>(unexpected.value()));
    }

    template <typename TOtherError,
              detail::EnableIf<std::is_constructible<ErrorType, TOtherError&&>::value &&
                                   !std::is_convertible<TOtherError&&, TError>::value,
                               bool> = false>
    explicit Expected(Unexpected<TOtherError> unexpected) noexcept(
        std::is_nothrow_constructible<ErrorType, TOtherError&&>::value) {
        new (reinterpret_cast<void*>(&mError)) TError(std::forward<TOtherError>(unexpected.value()));
    }

    // Destructor
    ~Expected() noexcept { reset(); }

    // Assignment operators
    Expected&
    operator=(const Expected& other) noexcept(std::is_nothrow_assignable<ValueType, ValueType>::value&&
                                                  std::is_nothrow_assignable<ErrorType, ErrorType>::value) {
        static_assert(
            std::is_copy_assignable<ValueType>::value && std::is_copy_constructible<ValueType>::value &&
                std::is_copy_assignable<ErrorType>::value && std::is_copy_constructible<ErrorType>::value,
            "The underlying type of Expected must be copy-constructible and copy-assignable");
        if (mHasValue && other.mHasValue) {
            mValue = other.mValue;
        } else {
            reset();
            if (other.mHasValue) {
                construct(other.mValue);
            } else {
                constructError(other.mError);
            }
        }
        return *this;
    }

    Expected&
    operator=(Expected&& other) noexcept(std::is_nothrow_assignable<ValueType, ValueType>::value&&
                                             std::is_nothrow_assignable<ErrorType, ErrorType>::value) {
        static_assert(
            std::is_move_assignable<ValueType>::value && std::is_move_constructible<ValueType>::value &&
                std::is_move_assignable<ErrorType>::value && std::is_move_constructible<ErrorType>::value,
            "The underlying type of Expected must be move-constructible and move-assignable");
        if (mHasValue && other.mHasValue) {
            mValue = std::move(other.mValue);
        } else {
            reset();
            if (other.mHasValue) {
                construct(std::move(other.mValue));
            } else {
                constructError(std::move(other.mError));
            }
        }
        return *this;
    }

    template <typename TOther = ValueType>
    detail::EnableIf<!std::is_same<Expected<TRaw>, typename std::decay<TOther>::type>::value &&
                         std::is_constructible<ValueType, TOther>::value &&
                         !(std::is_scalar<ValueType>::value &&
                           std::is_same<ValueType, typename std::decay<TOther>::type>::value) &&
                         std::is_assignable<ValueType&, TOther>::value,
                     Expected&>
    operator=(TOther&& value) noexcept(std::is_nothrow_constructible<ValueType, TOther>::value) {
        if (mHasValue) {
            mValue = std::forward<TOther>(value);
        } else {
            construct(std::forward<TOther>(value));
        }
        return *this;
    }

    template <typename TOther, typename TOtherError>
    detail::EnableIf<(!std::is_same<TOther, TRaw>::value || !std::is_same<ErrorType, TOtherError>::value) &&
                         std::is_constructible<ValueType, const TOther&>::value &&
                         std::is_constructible<ErrorType, const TOtherError&>::value &&
                         std::is_assignable<ValueType&, TOther>::value &&
                         std::is_assignable<ErrorType&, TOtherError>::value &&
                         !IsConstructibleOrConvertibleFrom<TOther>() && !IsAssignableFrom<TOther>(),
                     Expected&>
    operator=(const Expected<TOther, TOtherError>& other) {
        if (mHasValue && other.mHasValue) {
            mValue = other.mValue;
        } else if (!mHasValue && !other.mHasValue) {
            mError = other.mError;
        } else {
            reset();
            if (other.mHasValue) {
                construct(other.mValue);
            } else {
                constructError(other.mError);
            }
        }
        return *this;
    }

    template <typename TOther, typename TOtherError>
    detail::EnableIf<(!std::is_same<TOther, TRaw>::value || !std::is_same<ErrorType, TOtherError>::value) &&
                         std::is_constructible<ValueType, TOther>::value &&
                         std::is_assignable<ValueType&, TOther>::value &&
                         !IsConstructibleOrConvertibleFrom<TOther>() && !IsAssignableFrom<TOther>(),
                     Expected&>
    operator=(Expected<TOther>&& other) noexcept(std::is_nothrow_move_constructible<ValueType>::value&&
                                                     std::is_nothrow_move_assignable<ValueType>::value) {
        if (mHasValue && other.mHasValue) {
            mValue = std::move(other.mValue);
        } else if (!mHasValue && !other.mHasValue) {
            mError = std::move(other.mError);
        } else {
            reset();
            if (other.mHasValue) {
                construct(std::move(other.mValue));
            } else {
                constructError(std::move(other.mError));
            }
        }
        return *this;
    }

    // Modifiers
    template <typename... TArgs,
              typename = detail::EnableIf<std::is_constructible<ValueType, TArgs...>::value>>
    ValueType& emplace(TArgs&&... args) noexcept(std::is_nothrow_constructible<ValueType, TArgs...>::value) {
        reset();
        construct(std::forward<TArgs>(args)...);
        return mValue;
    }

    template <typename TOther,
              typename... TArgs,
              typename = detail::EnableIf<
                  std::is_constructible<ValueType, std::initializer_list<TOther>&, TArgs&&...>::value>>
    ValueType& emplace(std::initializer_list<TOther> list,
                       TArgs&&... args) noexcept(std::is_nothrow_constructible<ValueType, TArgs...>::value) {
        reset();
        construct(list, std::forward<TArgs>(args)...);
        return mValue;
    }

    void swap(Expected& other) noexcept(std::is_nothrow_move_constructible<ValueType>::value&& noexcept(
        swapDetail::adlSwap(std::declval<T&>(), std::declval<T&>()))) {
        using std::swap;
        if (mHasValue && other.mHasValue) {
            swap(mValue, other.mValue);
        } else if (!mHasValue && !other.mHasValue) {
            swap(mError, other.mError);
        } else {
            if (mHasValue) {
                ValueType tmp = mValue;
                mValue.~ValueType();
                constructError(other.mError);

                other.mError.~ErrorType();
                other.construct(std::move(tmp));
                assert(!mHasValue);
                assert(other.mHasValue);
            } else {
                ValueType tmp = other.mValue;
                other.mValue.~ValueType();
                other.constructError(mError);

                mError.~ErrorType();
                construct(std::move(tmp));
                assert(mHasValue);
                assert(!other.mHasValue);
            }
        }
    }

    // Observers
    explicit operator bool() const noexcept { return mHasValue; }

    bool operator!() const noexcept { return !mHasValue; }

    bool hasValue() const noexcept { return mHasValue; }

    TConstPtr operator->() const noexcept {
        assert(mHasValue);
        return &mValue;
    }

    TPtr operator->() noexcept {
        assert(mHasValue);
        return &mValue;
    }

    TConstRef operator*() const& noexcept {
        assert(mHasValue);
        return mValue;
    }

    TRef operator*() & noexcept {
        assert(mHasValue);
        return mValue;
    }

    ValueType&& operator*() && noexcept {
        assert(mHasValue);
        return std::move(mValue);
    }

    const ValueType& value() const& noexcept(false) {
        if (!mHasValue) {
            throw BadExpectedAccess();
        }
        return mValue;
    }

    ValueType& value() & noexcept(false) {
        if (!mHasValue) {
            throw BadExpectedAccess();
        }
        return mValue;
    }

    ValueType&& value() && noexcept(false) {
        if (!mHasValue) {
            throw BadExpectedAccess();
        }
        return std::move(mValue);
    }

    const ErrorType& error() const& noexcept(false) {
        if (mHasValue) {
            throw BadExpectedAccess();
        }
        return mError;
    }

    ErrorType& error() & noexcept(false) {
        if (mHasValue) {
            throw BadExpectedAccess();
        }
        return mError;
    }

    ErrorType&& error() && noexcept(false) {
        if (mHasValue) {
            throw BadExpectedAccess();
        }
        return std::move(mError);
    }

    template <typename TOther>
    auto valueOr(TOther&& value) const noexcept(std::is_nothrow_constructible<TRaw, TOther>::value)
        -> detail::EnableIf<std::is_constructible<TRaw, TOther>::value, TRaw> {
        return mHasValue ? mValue : static_cast<TRaw>(std::forward<TOther>(value));
    }

private:
    template <typename... TArgs>
    void construct(TArgs&&... args) noexcept(std::is_nothrow_constructible<ValueType, TArgs...>::value) {
        new (reinterpret_cast<void*>(&mValue)) ValueType(std::forward<TArgs>(args)...);
        mHasValue = true;
    }

    template <typename... TArgs>
    void constructError(TArgs&&... args) noexcept(std::is_nothrow_constructible<ErrorType, TArgs...>::value) {
        new (reinterpret_cast<void*>(&mError)) ErrorType(std::forward<TArgs>(args)...);
        mHasValue = false;
    }

    void reset() noexcept {
        if (mHasValue) {
            mHasValue = false;
            mValue.~ValueType();
        } else {
            mError.~ErrorType();
        }
    }

    union {
        ErrorType mError;
        ValueType mValue;
    };
    bool mHasValue = false;

    template <typename TValueOther, typename TErrorOther>
    friend class Expected;

    detail::Conditional<std::is_copy_assignable<T>::value && std::is_copy_constructible<T>::value,
                        detail::Copyable,
                        detail::Noncopyable>
        mCopyController;

    detail::Conditional<std::is_move_assignable<T>::value && std::is_move_constructible<T>::value,
                        detail::Movable,
                        detail::Nonmovable>
        mMoveController;
};

// Compare Expected<T, TError> to Expected<T, TError>
template <typename T, typename TError>
constexpr bool operator==(const Expected<T, TError>& x, const Expected<T, TError>& y) {
    return bool(x) != bool(y) ? false : bool(x) == false ? x.error() == y.error() : *x == *y;
}

template <typename T, typename TError>
constexpr bool operator!=(const Expected<T, TError>& x, const Expected<T, TError>& y) {
    return !(x == y);
}

template <typename T, typename TError>
constexpr bool operator<(const Expected<T, TError>& x, const Expected<T, TError>& y) {
    return (!y) ? false : (!x) ? true : *x < *y;
}

template <typename T, typename TError>
constexpr bool operator>(const Expected<T, TError>& x, const Expected<T, TError>& y) {
    return (y < x);
}

template <typename T, typename TError>
constexpr bool operator<=(const Expected<T, TError>& x, const Expected<T, TError>& y) {
    return !(y < x);
}

template <typename T, typename TError>
constexpr bool operator>=(const Expected<T, TError>& x, const Expected<T, TError>& y) {
    return !(x < y);
}

// Compare Expected<T, TError> to T
template <typename T, typename TError>
constexpr bool operator==(const Expected<T, TError>& x, const T& v) {
    return bool(x) ? *x == v : false;
}

template <typename T, typename TError>
constexpr bool operator==(const T& v, const Expected<T, TError>& x) {
    return bool(x) ? v == *x : false;
}

template <typename T, typename TError>
constexpr bool operator!=(const Expected<T, TError>& x, const T& v) {
    return bool(x) ? *x != v : true;
}

template <typename T, typename TError>
constexpr bool operator!=(const T& v, const Expected<T, TError>& x) {
    return bool(x) ? v != *x : true;
}

template <typename T, typename TError>
constexpr bool operator<(const Expected<T, TError>& x, const T& v) {
    return bool(x) ? *x < v : true;
}

template <typename T, typename TError>
constexpr bool operator>(const T& v, const Expected<T, TError>& x) {
    return bool(x) ? v > *x : true;
}

template <typename T, typename TError>
constexpr bool operator>(const Expected<T, TError>& x, const T& v) {
    return bool(x) ? *x > v : false;
}

template <typename T, typename TError>
constexpr bool operator<(const T& v, const Expected<T, TError>& x) {
    return bool(x) ? v < *x : false;
}

template <typename T, typename TError>
constexpr bool operator>=(const Expected<T, TError>& x, const T& v) {
    return bool(x) ? *x >= v : false;
}

template <typename T, typename TError>
constexpr bool operator<=(const T& v, const Expected<T, TError>& x) {
    return bool(x) ? v <= *x : false;
}

template <typename T, typename TError>
constexpr bool operator<=(const Expected<T, TError>& x, const T& v) {
    return bool(x) ? *x <= v : true;
}

template <typename T, typename TError>
constexpr bool operator>=(const T& v, const Expected<T, TError>& x) {
    return bool(x) ? v >= *x : true;
}

template <class T, class TError>
constexpr bool operator==(const Expected<T, TError>& x, const Unexpected<TError>& e) {
    return bool(x) ? false : x.error() == e.value();
}

template <class T, class TError>
constexpr bool operator==(const Unexpected<TError>& e, const Expected<T, TError>& x) {
    return x == e;
}

template <class T, class TError>
constexpr bool operator!=(const Expected<T, TError>& x, const Unexpected<TError>& e) {
    return bool(x) ? true : x.error() != e.value();
}

template <class T, class TError>
constexpr bool operator!=(const Unexpected<TError>& e, const Expected<T, TError>& x) {
    return x != e;
}

template <class T, class TError>
constexpr bool operator<(const Expected<T, TError>& x, const Unexpected<TError>& e) {
    return bool(x) ? false : x.error() < e.value();
}

template <class T, class TError>
constexpr bool operator>(const Expected<T, TError>& x, const Unexpected<TError>& e) {
    return bool(x) ? true : x.error() > e.value();
}

template <class T, class TError>
constexpr bool operator<=(const Expected<T, TError>& x, const Unexpected<TError>& e) {
    return bool(x) ? false : x.error() <= e.value();
}

template <class T, class TError>
constexpr bool operator>=(const Expected<T, TError>& x, const Unexpected<TError>& e) {
    return bool(x) ? true : x.error() >= e.value();
}

} // namespace libExpected

namespace std {

template <typename T, typename TError>
struct hash<libExpected::Expected<T, TError>> {
    using argument_type = libExpected::Expected<T, TError>;
    using result_type = std::size_t;

    result_type operator()(const argument_type& o) const
        noexcept(noexcept(hash<T>{}(*o)) && noexcept(hash<TError>{}(o.error()))) {
        return o.hasValue() ? std::hash<T>{}(o.value()) : std::hash<TError>{}(o.error());
    }
};

template <typename T, typename TError>
void swap(libExpected::Expected<T, TError>& lhs, libExpected::Expected<T, TError>& rhs) {
    lhs.swap(rhs);
}

} // namespace std

#endif // LIB_EXPECTED_EXPECTED_HPP_
