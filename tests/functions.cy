
a = 10; funky = f(b) { a + b };
a = 0; assert funky(3) == 13;

sum = f(x, y, z) {
    x + y + z
};
assert sum(1, 10, 100) == 111;
