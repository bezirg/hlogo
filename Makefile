default:
	runghc Setup.lhs configure --enable-tests
	runghc Setup.lhs build
