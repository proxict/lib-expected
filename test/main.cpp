#include "lib-expected/expected.hpp"

#include <gmock/gmock.h>
#include <sstream>
#include <string>

using namespace libExpected;

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
        Expected<Aware> v = makeUnexpected<Aware>("unexpected");
        EXPECT_FALSE(bool(v));
        EXPECT_EQ("unexpected", v.error());
    }
    {
        Expected<Aware, std::wstring> v = makeUnexpected<Aware>(L"unexpected");
        EXPECT_FALSE(bool(v));
        EXPECT_EQ(L"unexpected", v.error());
    }
    {
        Expected<Aware, int> v(makeUnexpected<Aware>(0));
        EXPECT_FALSE(bool(v));
    }
    EXPECT_FALSE(getState());
}

TEST_F(ExpectedTestAware, copyCtor) {
    {
        Expected<Aware> v(getState());
        Expected<Aware> vc(v);
        EXPECT_TRUE(bool(vc));
        EXPECT_FALSE(getState().moveCtor);
        EXPECT_FALSE(getState().copyAssigned);
        EXPECT_TRUE(getState().copyCtor);
    }
    {
        getState() = false;
        Expected<Aware, int> v(makeUnexpected<Aware>(0));
        Expected<Aware, int> vc(v);
        EXPECT_FALSE(bool(vc));
        EXPECT_FALSE(getState());
    }
}

TEST_F(ExpectedTestAware, moveCtor) {
    {
        Expected<Aware> v(getState());
        Expected<Aware> vc(std::move(v));
        EXPECT_TRUE(bool(vc));
        EXPECT_FALSE(getState().copyAssigned);
        EXPECT_FALSE(getState().copyCtor);
        EXPECT_TRUE(getState().moveCtor);
    }
    {
        getState() = false;
        Expected<Aware, int> v(makeUnexpected<Aware>(0));
        Expected<Aware, int> vc(std::move(v));
        EXPECT_FALSE(bool(vc));
        EXPECT_FALSE(getState());
    }
}

TEST_F(ExpectedTestAware, copyAssign) {
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
        Expected<Aware, int> v(makeUnexpected<Aware>(0));
        Expected<Aware, int> vc(makeUnexpected<Aware>(0));
        EXPECT_FALSE(bool(v));
        EXPECT_FALSE(bool(vc));
        vc = v;
        EXPECT_FALSE(bool(vc));
        EXPECT_FALSE(getState());
    }
    {
        getState() = false;
        Expected<Aware, int> v(makeUnexpected<Aware>(0));
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
}

TEST_F(ExpectedTestAware, moveAssign) {
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
        Expected<Aware, int> v(makeUnexpected<Aware>(0));
        Expected<Aware, int> vc(makeUnexpected<Aware>(0));
        EXPECT_FALSE(bool(v));
        EXPECT_FALSE(bool(vc));
        vc = std::move(v);
        EXPECT_FALSE(bool(vc));
        EXPECT_FALSE(getState());
    }
    {
        getState() = false;
        Expected<Aware, int> v(makeUnexpected<Aware>(0));
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
}

TEST(ExpectedTest, convertingCopyCtor) {
    Expected<std::string> a("This is a test string");
    Expected<std::ostringstream> b(a);
    EXPECT_TRUE(bool(a));
    EXPECT_TRUE(bool(b));
    EXPECT_EQ(*a, b->str());
}

TEST(ExpectedTest, convertingMoveCtor) {
    Expected<std::string> a("This is a test string");
    Expected<std::ostringstream> b(std::move(a));
    EXPECT_TRUE(bool(a));
    EXPECT_TRUE(bool(b));
    EXPECT_EQ(*a, b->str());
}

TEST(ExpectedTest, inPlaceCtor) {
    struct S {
        S(int a, int b, std::string str)
            : a(a)
            , b(b)
            , str(std::move(str)) {}

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
    a = makeUnexpected<Aware>("");
    EXPECT_FALSE(bool(a));
    EXPECT_TRUE(getState().dtor);
}

TEST_F(ExpectedTestAware, emplace) {
    {
        Expected<Aware, int> a(makeUnexpected<Aware>(0));
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
        Expected<std::string> a(makeUnexpected<std::string>("A"));
        Expected<std::string> b(makeUnexpected<std::string>("B"));
        std::swap(a, b);
        EXPECT_FALSE(bool(a));
        EXPECT_FALSE(bool(b));
        EXPECT_EQ(a.error(), "B");
        EXPECT_EQ(b.error(), "A");
    }
    {
        Expected<std::string> a("A");
        Expected<std::string> b(makeUnexpected<std::string>("unexpected"));
        std::swap(a, b);
        EXPECT_FALSE(bool(a));
        EXPECT_TRUE(bool(b));
        EXPECT_EQ(a.error(), "unexpected");
        EXPECT_EQ(*b, "A");
    }
    {
        Expected<std::string> a("A");
        Expected<std::string> b(makeUnexpected<std::string>("unexpected"));
        std::swap(b, a);
        EXPECT_FALSE(bool(a));
        EXPECT_TRUE(bool(b));
        EXPECT_EQ(a.error(), "unexpected");
        EXPECT_EQ(*b, "A");
    }
}

TEST(ExpectedTest, hasValue) {
    Expected<int> v(1);
    EXPECT_TRUE(v.hasValue());
}

TEST(ExpectedTest, value) {
    Expected<int> v(3);
    EXPECT_EQ(v.value(), 3);
}

TEST(ExpectedTest, BadExpectedAccess) {
    {
        Expected<int> v(1);
        EXPECT_THROW(v.error(), BadExpectedAccess);
    }
    {
        Expected<int> v(makeUnexpected<int>(""));
        EXPECT_THROW(v.value(), BadExpectedAccess);
    }
}

TEST_F(ExpectedTestAware, moveFrom) {
    Expected<Aware> a(getState());
    Expected<Aware> b(std::move(a.value()));
    EXPECT_TRUE(getState().moveCtor);
}

TEST(ExpectedTest, valueOr) {
    Expected<int> a(5);
    EXPECT_EQ(5, a.valueOr(3));
    a = makeUnexpected<int>("");
    EXPECT_EQ(3, a.valueOr(3));
}

TEST(ExpectedTest, equality) {
    Expected<int> oN(makeUnexpected<int>(""));
    Expected<int> o0(0);
    Expected<int> o1(1);

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

    EXPECT_TRUE(0 > oN);
    EXPECT_TRUE(1 > oN);
    EXPECT_FALSE(0 > o0);
    EXPECT_TRUE(1 > o0);
    EXPECT_FALSE(0 > o1);
    EXPECT_FALSE(1 > o1);

    EXPECT_FALSE(0 <= oN);
    EXPECT_FALSE(1 <= oN);
    EXPECT_TRUE(0 <= o0);
    EXPECT_FALSE(1 <= o0);
    EXPECT_TRUE(0 <= o1);
    EXPECT_TRUE(1 <= o1);
}

TEST(ExpectedTest, references) {
    int v = 1;
    Expected<int&> ev(v);
    EXPECT_EQ(1, *ev);
    v = 3;
    EXPECT_EQ(3, *ev);
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
                return makeUnexpected<int>("unexpected");
            }
        }

        Expected<std::string> getStr(const std::string& str) {
            return str;
        }
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
