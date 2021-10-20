import unittest
from TestPipe import static_test

class TestTestPipe(unittest.TestCase):
    def test_static_test(self):
        self.assertTupleEqual(static_test([1,2,3],[1,2,3]),(0,0,0,0))
        self.assertTupleEqual(static_test([1,2,3],[1,1,1]),(1,38.88888888888889,1.6666666666,1.29099444))
        self.assertTupleEqual(static_test([12,13,19],[13.5,0,12]),(7.16666666666667,49.780701754386,73.4166666666667,8.56835262268464))

    def test_exceptions(self):
        self.assertRaises(ZeroDivisionError, static_test,*([0,1,2],[1,2,3]))
        self.assertRaises(AssertionError, static_test, *([1,2],[1,2,3]))

    def assertTupleEqual(self, tp1, tp2):
        for i,j in zip(tp1, tp2):
            self.assertAlmostEqual(i,j)
