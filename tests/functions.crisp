
b = 10; closure = f(a) { a + b };
b = 0; assert closure(5) == 15;

sum = f(x, y, z) {
    x + y + z
}; assert sum(1, 10, 100) == 111;

nestedif = f(b) {
    if b {
        if !b {
            assert false
        } else {
            assert true
        }
    } else {
        assert false
    }
}; nestedif(true);

rec = f(x) {
    if x < 0 {
        true;
    } else {
        y = x-1;
        rec(y);
    }
}; assert rec(10);