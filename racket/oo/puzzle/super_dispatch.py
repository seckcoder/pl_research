class c1(object):
    def m1(self):
        self.m2() # dynamic dispatch

    def m2(self):
        print "c1:m2"

class c2(c1):
    def m1(self):
        print "c2:m1"

    def m2(self):
        print "c2:m2"

    def m3(self):
        super(c2, self).m1() # static dispatch

class c3(c2):
    def m1(self):
        print "c3:m1"

    def m2(self):
        print "c3:m2"


o3 = c3()
o3.m3()
