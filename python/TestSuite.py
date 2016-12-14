import quantum as Q

def testInitial():
    base = Q.Register(2)
    compare = Q.Register(2)
    compare.injectState([1, 0, 0, 0])
    if base != compare:
        raise Exception("Failed Register Initialize test")

def testX():
    compare = Q.Register(2)
    compare.injectState([0, 0, 1, 0])

    base = Q.Register(2)
    base.X(1)
    if base != compare:
        raise Exception("Failed X Gate test: ", str(base), " != ", str(compare))

def testY():
    compare = Q.Register(2)
    compare.injectState([0, 0, 0 + 1j, 0])

    base = Q.Register(2)
    base.Y(1)
    if base != compare:
        raise Exception("Failed X Gate test: ", str(base), " != ", str(compare))

def testZ():
    compare = Q.Register(2)
    compare.injectState([1, 0, 0, 0])

    base = Q.Register(2)
    base.Z(1)
    if base != compare:
        raise Exception("Failed X Gate test: ", str(base), " != ", str(compare))

def testHadamard():
    sqrt2 = 1/(2**.5)
    compare = Q.Register(2)
    compare.injectState([sqrt2, sqrt2, 0, 0])

    base = Q.Register(2)
    base.hadamard(0)
    if base != compare:
        raise Exception("Failed X Gate test: ", str(base), " != ", str(compare))

def testBellState():
    sqrt2 = 1/(2**.5)
    compare = Q.Register(2)
    compare.injectState([sqrt2, 0, 0, sqrt2])

    base = Q.Register(2)
    base.hadamard(1)
    base.cnot(1, 0)
    if base != compare:
        raise Exception("Failed X Gate test: ", str(base), " != ", str(compare))

def testTooLarge():
    try:
        base = Q.Register(500)
    except:
        return True

    raise Exception("Failed to Raise exception on too large Register")

if __name__ == "__main__":
    testInitial()
    testX()
    testY()
    testZ()
    testHadamard()
    testBellState()
    testTooLarge()
