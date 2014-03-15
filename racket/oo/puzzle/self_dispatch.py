class c1:
    def m1(self):
        print "c1:m1"

    def m2(self):
        self.m1() # python use dynamic dispatch for self!


class c2(c1):
    def m1(self):
        print "c2:m1"


v = c2()
v.m2()
