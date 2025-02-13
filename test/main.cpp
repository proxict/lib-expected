#include "lib-expected/expected.hpp"

#include <gmock/gmock.h>
#include <sstream>
#include <string>

using namespace libExpected;

struct A {};
struct B : A {};

class ExpectedTestAware : public ::testing::Test {
protected:
    struct State {
        bool ctor;
        bool dtor;
        bool copyAssigned;
        bool copyCtor;
        bool moveAssigned;
        bool moveCtor;

        State& operator=(bool state) {
            ctor = state;
            dtor = state;
            copyAssigned = state;
            copyCtor = state;
            moveAssigned = state;
            moveCtor = state;
            return *this;
        }

        bool operator==(const bool v) const {
            return ctor == v && dtor == v && copyAssigned == v && copyCtor == v && moveAssigned == v &&
                   moveCtor == v;
        }

        operator bool() const { return *this == true; }

        State() { *this = false; }
    };

    class Aware final {
    public:
        Aware(State& state)
            : mState(state) {
            state.ctor = true;
        }

        ~Aware() { mState.dtor = true; }

        Aware(const Aware& other)
            : mState(other.mState) {
            mState.copyCtor = true;
        }

        Aware(Aware&& other)
            : mState(other.mState) {
            mState.moveCtor = true;
        }

        Aware& operator=(const Aware&) {
            mState.copyAssigned = true;
            return *this;
        }

        Aware& operator=(Aware&&) {
            mState.moveAssigned = true;
            return *this;
        }

    private:
        State& mState;
    };

    virtual void SetUp() override { mState = false; }

    virtual void TearDown() override { mState = false; }

    State& getState() { return mState; }

    const State& getState() const { return mState; }

private:
    State mState;
};

template <typename TWhat, typename TFrom>
class IsImplicitlyConstructibleFrom {
    static std::false_type test(...);

    static std::true_type test(TWhat);

public:
    static constexpr bool value = decltype(test(std::declval<TFrom>()))::value;
};

TEST_F(ExpectedTestAware, ctor) {
    {
        Expected<int> v(1);
        EXPECT_TRUE(bool(v));
        EXPECT_EQ(1, *v);
    }
    {
        Expected<Aware> v(getState());
        EXPECT_TRUE(bool(v));
        EXPECT_TRUE(getState().ctor);
    }
}

TEST_F(ExpectedTestAware, unexpected) {
    {
        Expected<Aware> v = makeUnexpected("unexpected");
        EXPECT_FALSE(bool(v));
        EXPECT_EQ("unexpected", v.error());
    }
    {
        Expected<Aware, std::wstring> v = makeUnexpected(L"unexpected");
        EXPECT_FALSE(bool(v));
        EXPECT_EQ(L"unexpected", v.error());
    }
    {
        Expected<Aware, int> v(makeUnexpected(0));
        EXPECT_FALSE(bool(v));
    }
    {
        Expected<Aware, std::ostringstream> v(makeUnexpected(std::string("")));
        EXPECT_FALSE(bool(v));
    }
    EXPECT_FALSE(getState());
    {
        Unexpected<int> u(1);
        EXPECT_EQ(u.value(), 1);
    }
    {
        const Unexpected<int> u(1);
        EXPECT_EQ(u.value(), 1);
    }
    EXPECT_EQ(Unexpected<int>(1).value(), 1);
}

TEST_F(ExpectedTestAware, copyCtor) {
    Expected<Aware> a(getState());
    EXPECT_TRUE(bool(a));
    EXPECT_FALSE(getState().copyCtor);
    EXPECT_FALSE(getState().copyAssigned);
    EXPECT_FALSE(getState().moveCtor);
    getState() = false;

    Expected<Aware> b(a);
    EXPECT_FALSE(getState().ctor);
    EXPECT_FALSE(getState().copyAssigned);
    EXPECT_TRUE(getState().copyCtor);
    getState() = false;

    const Expected<Aware> u(makeUnexpected(""));
    Expected<Aware> c(u);
    EXPECT_FALSE(getState().ctor);
    EXPECT_FALSE(getState().copyAssigned);
    EXPECT_FALSE(getState().copyCtor);
}

TEST_F(ExpectedTestAware, moveCtor) {
    Expected<Aware> a(getState());
    EXPECT_TRUE(bool(a));
    EXPECT_FALSE(getState().copyCtor);
    getState() = false;

    Expected<Aware> b(std::move(a));
    EXPECT_FALSE(getState().ctor);
    EXPECT_FALSE(getState().copyCtor);
    EXPECT_FALSE(getState().copyAssigned);
    EXPECT_FALSE(getState().moveAssigned);
    EXPECT_TRUE(getState().moveCtor);
    getState() = false;

    Expected<Aware> u(makeUnexpected(""));
    Expected<Aware> c(std::move(u));
    EXPECT_FALSE(getState().ctor);
    EXPECT_FALSE(getState().copyCtor);
    EXPECT_FALSE(getState().copyAssigned);
    EXPECT_FALSE(getState().moveAssigned);
    EXPECT_FALSE(getState().moveCtor);
}

TEST_F(ExpectedTestAware, copyAssign) {
    {
        const int v = 1;
        Expected<int> vc(0);
        vc = v;
        EXPECT_TRUE(bool(vc));
        EXPECT_EQ(*vc, 1);
    }
    {
        const int v = 1;
        Expected<int> vc(makeUnexpected(""));
        vc = v;
        EXPECT_TRUE(bool(vc));
        EXPECT_EQ(*vc, 1);
    }
    {
        Expected<int> v(makeUnexpected(""));
        Expected<int> vc(0);
        vc = v;
        EXPECT_FALSE(bool(vc));
    }
    {
        Expected<int> v(1);
        Expected<int> vc(0);
        vc = v;
        EXPECT_TRUE(bool(vc));
        EXPECT_EQ(*vc, 1);
    }
    {
        Expected<int> v(1);
        Expected<int> vc(makeUnexpected(""));
        vc = v;
        EXPECT_TRUE(bool(vc));
        EXPECT_EQ(*vc, 1);
    }
    {
        Expected<Aware> v(getState());
        Expected<Aware> vc(getState());
        vc = v;
        EXPECT_TRUE(bool(vc));
        EXPECT_FALSE(getState().moveCtor);
        EXPECT_FALSE(getState().copyCtor);
        EXPECT_TRUE(getState().copyAssigned);
    }
    {
        getState() = false;
        Expected<Aware, int> v(makeUnexpected(0));
        Expected<Aware, int> vc(makeUnexpected(0));
        EXPECT_FALSE(bool(v));
        EXPECT_FALSE(bool(vc));
        vc = v;
        EXPECT_FALSE(bool(vc));
        EXPECT_FALSE(getState());
    }
    {
        getState() = false;
        Expected<Aware, int> v(makeUnexpected(0));
        Expected<Aware, int> vc(getState());
        EXPECT_TRUE(bool(vc));
        vc = v;
        EXPECT_FALSE(bool(v));
        EXPECT_FALSE(bool(vc));
        EXPECT_FALSE(getState().copyCtor);
        EXPECT_FALSE(getState().copyAssigned);
        EXPECT_TRUE(getState().ctor);
        EXPECT_TRUE(getState().dtor);
    }
    {
        Expected<A*> a(makeUnexpected(""));
        Expected<B*> b(nullptr);
        a = b;
        EXPECT_TRUE(a);
        EXPECT_TRUE(b);
    }
    {
        A ao;
        Expected<A*> a(&ao);
        Expected<B*> b(nullptr);
        a = b;
        EXPECT_TRUE(a);
        EXPECT_TRUE(b);
    }
    {
        Expected<A*> a(makeUnexpected("foo"));
        Expected<B*> b(makeUnexpected("bar"));
        a = b;
        EXPECT_FALSE(a);
        EXPECT_FALSE(b);
        EXPECT_EQ(a.error(), "bar");
    }
    {
        A ao;
        Expected<A*> a(&ao);
        Expected<B*> b(makeUnexpected("bar"));
        a = b;
        EXPECT_FALSE(a);
        EXPECT_FALSE(b);
        EXPECT_EQ(a.error(), "bar");
    }
}

TEST_F(ExpectedTestAware, moveAssign) {
    {
        int v = 1;
        Expected<int> vc(0);
        vc = std::move(v);
        EXPECT_TRUE(bool(vc));
        EXPECT_EQ(*vc, 1);
    }
    {
        int v = 1;
        Expected<int> vc(makeUnexpected(""));
        vc = std::move(v);
        EXPECT_TRUE(bool(vc));
        EXPECT_EQ(*vc, 1);
    }
    {
        Expected<int> v(makeUnexpected(""));
        Expected<int> vc(0);
        vc = std::move(v);
        EXPECT_FALSE(bool(vc));
    }
    {
        Expected<int> v(1);
        Expected<int> vc(0);
        vc = std::move(v);
        EXPECT_TRUE(bool(vc));
        EXPECT_EQ(*vc, 1);
    }
    {
        Expected<int> v(1);
        Expected<int> vc(makeUnexpected(""));
        vc = std::move(v);
        EXPECT_TRUE(bool(vc));
        EXPECT_EQ(*vc, 1);
    }
    {
        Expected<Aware> v(getState());
        Expected<Aware> vc(getState());
        vc = std::move(v);
        EXPECT_TRUE(bool(vc));
        EXPECT_FALSE(getState().moveCtor);
        EXPECT_FALSE(getState().copyCtor);
        EXPECT_FALSE(getState().copyAssigned);
        EXPECT_TRUE(getState().moveAssigned);
    }
    {
        getState() = false;
        Expected<Aware, int> v(makeUnexpected(0));
        Expected<Aware, int> vc(makeUnexpected(0));
        EXPECT_FALSE(bool(v));
        EXPECT_FALSE(bool(vc));
        vc = std::move(v);
        EXPECT_FALSE(bool(vc));
        EXPECT_FALSE(getState());
    }
    {
        getState() = false;
        Expected<Aware, int> v(makeUnexpected(0));
        Expected<Aware, int> vc(getState());
        EXPECT_TRUE(bool(vc));
        vc = std::move(v);
        EXPECT_FALSE(bool(v));
        EXPECT_FALSE(bool(vc));
        EXPECT_FALSE(getState().copyCtor);
        EXPECT_FALSE(getState().copyAssigned);
        EXPECT_FALSE(getState().moveCtor);
        EXPECT_FALSE(getState().moveAssigned);
        EXPECT_TRUE(getState().ctor);
        EXPECT_TRUE(getState().dtor);
    }
    {
        Expected<A*> a(makeUnexpected(""));
        Expected<B*> b(nullptr);
        a = std::move(b);
        EXPECT_TRUE(a);
        EXPECT_TRUE(b);
    }
    {
        A ao;
        Expected<A*> a(&ao);
        Expected<B*> b(nullptr);
        a = std::move(b);
        EXPECT_TRUE(a);
        EXPECT_TRUE(b);
    }
    {
        Expected<A*> a(makeUnexpected("foo"));
        Expected<B*> b(makeUnexpected("bar"));
        a = std::move(b);
        EXPECT_FALSE(a);
        EXPECT_FALSE(b);
        EXPECT_EQ(a.error(), "bar");
    }
    {
        A ao;
        Expected<A*> a(&ao);
        Expected<B*> b(makeUnexpected("bar"));
        a = std::move(b);
        EXPECT_FALSE(a);
        EXPECT_FALSE(b);
        EXPECT_EQ(a.error(), "bar");
    }
}

TEST(ExpectedTest, convertingCopyCtor) {
    {
        Expected<std::string> a("This is a test string");
        Expected<std::ostringstream> b(a);
        EXPECT_TRUE(bool(a));
        EXPECT_TRUE(bool(b));
        EXPECT_EQ(*a, b->str());
    }
    {
        Expected<std::string> a(makeUnexpected(""));
        Expected<std::ostringstream> b(a);
        EXPECT_FALSE(bool(a));
        EXPECT_FALSE(bool(b));
    }
    {
        Expected<B*> a(nullptr);
        Expected<A*> b(a);
        EXPECT_TRUE(bool(a));
        EXPECT_TRUE(bool(b));
    }
    {
        Expected<B*> a(makeUnexpected(""));
        Expected<A*> b(a);
        EXPECT_FALSE(bool(a));
        EXPECT_FALSE(bool(b));
    }
}

TEST(ExpectedTest, convertingMoveCtor) {
    {
        Expected<std::string> a("This is a test string");
        Expected<std::ostringstream> b(std::move(a));
        EXPECT_TRUE(bool(a));
        EXPECT_TRUE(bool(b));
        EXPECT_EQ(*a, b->str());
    }
    {
        Expected<std::string> a(makeUnexpected(""));
        Expected<std::ostringstream> b(std::move(a));
        EXPECT_FALSE(bool(a));
        EXPECT_FALSE(bool(b));
    }
    {
        Expected<B*> a(nullptr);
        Expected<A*> b(std::move(a));
        EXPECT_TRUE(bool(a));
        EXPECT_TRUE(bool(b));
    }
    {
        Expected<B*> a(makeUnexpected(""));
        Expected<A*> b(std::move(a));
        EXPECT_FALSE(bool(a));
        EXPECT_FALSE(bool(b));
    }
    {
        Expected<std::string> a(makeUnexpected(""));
        std::string b("dog");
        a = std::move(b);
        EXPECT_TRUE(bool(a));
        EXPECT_EQ(*a, "dog");
    }
    {
        Expected<std::string> a("cat");
        std::string b("dog");
        a = std::move(b);
        EXPECT_TRUE(bool(a));
        EXPECT_EQ(*a, "dog");
    }
}

TEST(ExpectedTest, inPlaceCtor) {
    struct S {
        S(int a_, int b_, std::string str_)
            : a(a_)
            , b(b_)
            , str(std::move(str_)) {}

        S(const S&) = delete;
        S& operator=(const S&) = delete;
        S(S&&) = delete;
        S& operator=(S&&) = delete;

        int a, b;
        std::string str;
    };
    Expected<S> v(InPlace, 1, 2, "InPlace");
    EXPECT_TRUE(bool(v));
    EXPECT_EQ(v->a, 1);
    EXPECT_EQ(v->b, 2);
    EXPECT_EQ(v->str, "InPlace");
}

TEST(ExpectedTest, inPlaceInitializerList) {
    Expected<std::string> v(InPlace, { 'a', 'b', 'c' });
    EXPECT_TRUE(bool(v));
    EXPECT_EQ(*v, "abc");
}

TEST(ExpectedTest, ctorValue) {
    std::string s("Psycho");
    Expected<std::string> v(s);
    EXPECT_TRUE(bool(v));
    EXPECT_EQ(*v, s);
}

TEST(ExpectedTest, ctorValueMove) {
    std::string s("Psycho");
    Expected<std::string> v(std::move(s));
    EXPECT_TRUE(bool(v));
    EXPECT_EQ(*v, std::string("Psycho"));
    EXPECT_TRUE(s.empty());
}

TEST_F(ExpectedTestAware, assignUnexpected) {
    Expected<Aware> a(getState());
    EXPECT_TRUE(bool(a));
    EXPECT_TRUE(getState().ctor);
    EXPECT_FALSE(getState().dtor);
    a = makeUnexpected("");
    EXPECT_FALSE(bool(a));
    EXPECT_TRUE(getState().dtor);
}

TEST_F(ExpectedTestAware, emplace) {
    {
        Expected<std::vector<int>> a(makeUnexpected(""));
        a.emplace({ 1, 2, 3 });
        EXPECT_TRUE(bool(a));
        EXPECT_EQ(*a, std::vector<int>({ 1, 2, 3 }));
    }
    {
        Expected<Aware, int> a(makeUnexpected(0));
        EXPECT_FALSE(bool(a));
        a.emplace(getState());
        EXPECT_TRUE(bool(a));
        EXPECT_TRUE(getState().ctor);
        EXPECT_FALSE(getState().copyCtor);
        EXPECT_FALSE(getState().moveCtor);
        EXPECT_FALSE(getState().copyAssigned);
        EXPECT_FALSE(getState().moveAssigned);
    }
    EXPECT_TRUE(getState().dtor);
}

TEST_F(ExpectedTestAware, swap) {
    {
        Expected<std::string> a("A");
        Expected<std::string> b("B");
        std::swap(a, b);
        EXPECT_TRUE(bool(a));
        EXPECT_TRUE(bool(b));
        EXPECT_EQ(*a, "B");
        EXPECT_EQ(*b, "A");
    }
    {
        Expected<std::string> a(makeUnexpected("A"));
        Expected<std::string> b(makeUnexpected("B"));
        std::swap(a, b);
        EXPECT_FALSE(bool(a));
        EXPECT_FALSE(bool(b));
        EXPECT_EQ(a.error(), "B");
        EXPECT_EQ(b.error(), "A");
    }
    {
        Expected<std::string> a("A");
        Expected<std::string> b(makeUnexpected("unexpected"));
        std::swap(a, b);
        EXPECT_FALSE(bool(a));
        EXPECT_TRUE(bool(b));
        EXPECT_EQ(a.error(), "unexpected");
        EXPECT_EQ(*b, "A");
    }
    {
        Expected<std::string> a("A");
        Expected<std::string> b(makeUnexpected("unexpected"));
        std::swap(b, a);
        EXPECT_FALSE(bool(a));
        EXPECT_TRUE(bool(b));
        EXPECT_EQ(a.error(), "unexpected");
        EXPECT_EQ(*b, "A");
    }
}

TEST(ExpectedTest, dereference) {
    {
        const Expected<int> v(1);
        EXPECT_EQ(*v, 1);
    }
    {
        int k = 1;
        const Expected<int&> v(k);
        EXPECT_EQ(*v, 1);
    }
    {
        const Expected<std::string> v("abc");
        EXPECT_EQ(v->size(), 3);
    }
    {
        std::string k("abc");
        const Expected<std::string&> v(k);
        EXPECT_EQ(v->size(), 3);
    }
    {
        std::string k("abc");
        Expected<std::string&> v(k);
        EXPECT_EQ(v->size(), 3);
    }
}

TEST(ExpectedTest, dereferenceRvalue) {
    EXPECT_EQ(*Expected<std::string>("psycho"), "psycho");
    EXPECT_EQ(Expected<std::string>("psycho")->size(), 6);
}

TEST(ExpectedTest, hasValue) {
    Expected<int> v(1);
    EXPECT_TRUE(v.hasValue());
}

TEST(ExpectedTest, value) {
    {
        Expected<int> v(3);
        EXPECT_EQ(v.value(), 3);
        v.value() = 4;
        EXPECT_EQ(v.value(), 4);
    }
    {
        const Expected<int> v(5);
        EXPECT_EQ(v.value(), 5);
    }
    {
        int v = 5;
        const Expected<int&> o(v);
        EXPECT_EQ(o.value(), 5);
    }
    {
        int v = 5;
        Expected<int&> o(v);
        EXPECT_EQ(o.value(), 5);
    }
    EXPECT_EQ(Expected<int>(1).value(), 1);
}

TEST(ExpectedTest, error) {
    {
        Expected<int> v(makeUnexpected("abc"));
        EXPECT_EQ(v.error(), "abc");
    }
    {
        const Expected<int> v(makeUnexpected("abc"));
        EXPECT_EQ(v.error(), "abc");
    }
    EXPECT_EQ(Expected<int>(makeUnexpected("abc")).error(), "abc");
}

TEST(ExpectedTest, BadExpectedAccess) {
    {
        Expected<int> v(makeUnexpected(""));
        EXPECT_THROW(v.value(), BadExpectedAccess);
    }
    {
        const Expected<int> v(makeUnexpected(""));
        EXPECT_THROW(v.value(), BadExpectedAccess);
    }
    {
        Expected<int&> v(makeUnexpected(""));
        EXPECT_THROW(v.value(), BadExpectedAccess);
    }
    {
        const Expected<int&> v(makeUnexpected(""));
        EXPECT_THROW(v.value(), BadExpectedAccess);
    }
    EXPECT_THROW(Expected<int>(makeUnexpected("")).value(), BadExpectedAccess);
    EXPECT_THROW(Expected<int>(1).error(), BadExpectedAccess);
    try {
        Expected<int>(makeUnexpected("")).value();
    } catch (const BadExpectedAccess& e) {
        EXPECT_NE(e.what(), nullptr);
    }
}

TEST(ExpectedTest, BadExpectedAccessError) {
    {
        Expected<int> v(1);
        EXPECT_THROW(v.error(), BadExpectedAccess);
    }
    {
        const Expected<int> v(1);
        EXPECT_THROW(v.error(), BadExpectedAccess);
    }
    EXPECT_THROW(Expected<int>(1).error(), BadExpectedAccess);
}

TEST_F(ExpectedTestAware, moveFrom) {
    Expected<Aware> a(getState());
    Expected<Aware> b(std::move(a.value()));
    EXPECT_TRUE(getState().moveCtor);
}

TEST(ExpectedTest, valueOr) {
    Expected<int> a(5);
    EXPECT_EQ(5, a.valueOr(3));
    a = makeUnexpected("");
    EXPECT_EQ(3, a.valueOr(3));
    {
        Expected<uint64_t> ov(makeUnexpected(""));
        EXPECT_EQ(ov.valueOr(1), 1);
    }
    {
        Expected<std::string> ov("abc");
        EXPECT_EQ(ov.valueOr("def"), "abc");
    }
    {
        int k = 42;
        int v = 1;
        Expected<int&> ov(k);
        int& r = ov.valueOr(v);
        EXPECT_EQ(42, r);
        EXPECT_EQ(1, v);
        r = 3;
        EXPECT_EQ(3, k);
        EXPECT_EQ(1, v);
    }
    {
        const int k = 42;
        const int v = 1;
        Expected<const int&> ov(k);
        const int& r = ov.valueOr(v);
        EXPECT_EQ(42, r);
        EXPECT_EQ(1, v);
        const_cast<int&>(r) = 3;
        EXPECT_EQ(3, k);
        EXPECT_EQ(1, v);
    }
    {
        int v = 1;
        Expected<int&> ov(makeUnexpected(""));
        int& r = ov.valueOr(v);
        EXPECT_EQ(1, r);
        EXPECT_EQ(1, v);
        r = 3;
        EXPECT_EQ(3, v);
    }
    {
        const int v = 1;
        Expected<const int&> ov(makeUnexpected(""));
        const int& r = ov.valueOr(v);
        EXPECT_EQ(1, r);
        EXPECT_EQ(1, v);
        const_cast<int&>(r) = 3;
        EXPECT_EQ(3, v);
    }
    {
        const B v;
        Expected<const A&> ov(makeUnexpected(""));
        const A& r = ov.valueOr(v);
        (void)r;
    }
    {
        B v;
        Expected<A&> ov(makeUnexpected(""));
        A& r = ov.valueOr(v);
        (void)r;
    }
}

TEST(ExpectedTest, equality) {
    Expected<int> oN(makeUnexpected(""));
    Expected<int> o0(0);
    Expected<int> o1(1);

    EXPECT_FALSE(o0 == o1);
    EXPECT_TRUE(o0 != o1);
    EXPECT_TRUE(o0 < o1);
    EXPECT_FALSE(o0 > o1);
    EXPECT_TRUE(o0 <= o1);
    EXPECT_FALSE(o0 >= o1);

    EXPECT_FALSE(o1 == 0);
    EXPECT_FALSE(0 == o1);
    EXPECT_TRUE(o1 != 0);
    EXPECT_TRUE(0 != o1);

    EXPECT_TRUE(oN < 0);
    EXPECT_TRUE(oN < 1);
    EXPECT_FALSE(o0 < 0);
    EXPECT_TRUE(o0 < 1);
    EXPECT_FALSE(o1 < 0);
    EXPECT_FALSE(o1 < 1);

    EXPECT_FALSE(oN >= 0);
    EXPECT_FALSE(oN >= 1);
    EXPECT_TRUE(o0 >= 0);
    EXPECT_FALSE(o0 >= 1);
    EXPECT_TRUE(o1 >= 0);
    EXPECT_TRUE(o1 >= 1);

    EXPECT_FALSE(oN > 0);
    EXPECT_FALSE(oN > 1);
    EXPECT_FALSE(o0 > 0);
    EXPECT_FALSE(o0 > 1);
    EXPECT_TRUE(o1 > 0);
    EXPECT_FALSE(o1 > 1);

    EXPECT_TRUE(oN <= 0);
    EXPECT_TRUE(oN <= 1);
    EXPECT_TRUE(o0 <= 0);
    EXPECT_TRUE(o0 <= 1);
    EXPECT_FALSE(o1 <= 0);
    EXPECT_TRUE(o1 <= 1);

    EXPECT_TRUE(0 < o1);
    EXPECT_TRUE(0 > oN);
    EXPECT_TRUE(1 > oN);
    EXPECT_FALSE(0 > o0);
    EXPECT_TRUE(1 > o0);
    EXPECT_FALSE(0 > o1);
    EXPECT_FALSE(1 > o1);

    EXPECT_TRUE(0 >= oN);
    EXPECT_FALSE(0 <= oN);
    EXPECT_FALSE(1 <= oN);
    EXPECT_TRUE(0 <= o0);
    EXPECT_FALSE(1 <= o0);
    EXPECT_TRUE(0 <= o1);
    EXPECT_TRUE(1 <= o1);

    EXPECT_FALSE(makeUnexpected("") == o1);
    EXPECT_TRUE(o1 != makeUnexpected(""));
    EXPECT_TRUE(makeUnexpected("") != o1);
    EXPECT_FALSE(o1 < makeUnexpected(""));
    EXPECT_TRUE(makeUnexpected("") < o1);
    EXPECT_FALSE(o1 <= makeUnexpected(""));
    EXPECT_TRUE(makeUnexpected("") <= o1);
    EXPECT_TRUE(o1 > makeUnexpected(""));
    EXPECT_FALSE(makeUnexpected("") > o1);
    EXPECT_TRUE(o1 >= makeUnexpected(""));
    EXPECT_FALSE(makeUnexpected("") >= o1);
}

TEST(ExpectedTest, references) {
    int v = 1;
    Expected<int&> ev(v);
    EXPECT_EQ(1, *ev);
    v = 3;
    EXPECT_EQ(3, *ev);
}

TEST(ExpectedTest, implicitConstructionFromReference) {
#define MY_EXPECT_TRUE(...) EXPECT_TRUE(([&]() { return __VA_ARGS__; }()))
#define MY_EXPECT_FALSE(...) EXPECT_FALSE(([&]() { return __VA_ARGS__; }()))
    MY_EXPECT_TRUE(IsImplicitlyConstructibleFrom<Expected<const A&>, A>::value);
    MY_EXPECT_TRUE(IsImplicitlyConstructibleFrom<Expected<const A&>, A&>::value);
    MY_EXPECT_TRUE(IsImplicitlyConstructibleFrom<Expected<const A&>, const A>::value);
    MY_EXPECT_TRUE(IsImplicitlyConstructibleFrom<Expected<const A&>, const A&>::value);
    MY_EXPECT_FALSE(IsImplicitlyConstructibleFrom<Expected<A&>, A>::value);
    MY_EXPECT_FALSE(IsImplicitlyConstructibleFrom<Expected<A&>, const A>::value);
    MY_EXPECT_FALSE(IsImplicitlyConstructibleFrom<Expected<A&>, const A&>::value);
    MY_EXPECT_TRUE(IsImplicitlyConstructibleFrom<Expected<const B&>, B&>::value);
    MY_EXPECT_TRUE(IsImplicitlyConstructibleFrom<Expected<const A&>, B&>::value);
    MY_EXPECT_FALSE(IsImplicitlyConstructibleFrom<Expected<const B&>, A&>::value);
#undef MY_EXPECT_TRUE
#undef MY_EXPECT_FALSE
}

TEST(ExpectedTest, hash) {
    Expected<int> v(3);
    EXPECT_EQ(std::hash<Expected<int>>{}(v), std::hash<int>{}(3));
}

TEST(ExpectedTest, usage) {
    struct S {
        Expected<int> get(const bool b) {
            if (b) {
                return 1;
            } else {
                return makeUnexpected("unexpected");
            }
        }

        Expected<std::string> getStr(const std::string& str) { return str; }
    };

    S s;
    {
        const Expected<int> res = s.get(true);
        EXPECT_TRUE(bool(res));
    }
    {
        const Expected<int> res = s.get(false);
        EXPECT_FALSE(bool(res));
    }
    {
        const Expected<std::string> res = s.getStr("test");
        EXPECT_TRUE(bool(res));
        EXPECT_EQ("test", *res);
    }
}

int main(int argc, char** argv) {
    ::testing::InitGoogleMock(&argc, argv);
    ::testing::FLAGS_gtest_death_test_style = "threadsafe";
    return RUN_ALL_TESTS();
}
