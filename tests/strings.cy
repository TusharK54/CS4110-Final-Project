a = "abc";
b = "123";
c = "\n";

str1 = "hello ";
str2 = "world";
str3 = "hello world";
assert str3 == str1 + str2;

str4 = 'hello ';
str5 = 'world';
str6 = 'hello world';
assert str4 + str5 == str6;

a = "abcdef";
assert a[3] == "d";
assert a[-1] == "f";
assert a[3:5] == "de";
assert a[-3:-1] == "de";
assert a[2:] == "cdef";
assert a[:-2] == "abcd";
assert a[:] == a;
assert a[2:] == a[2:()];
assert a[:-2] == a[():-2];
assert a[:] == a[():()];
