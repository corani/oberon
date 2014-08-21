extern "C" {
    // FUNTIONS
    int _ABS_I(int v) {
        return (v > 0 ? v : -v);
    }

    double _ABS_F(double v) {
        return (v > 0 ? v : -v);
    }

    bool _ODD(int v) {
        return (v % 2) == 1;
    }

    // Arithmetic shift by n
    int _ASH(int v, int n) {
        if (n > 0) {
            return v << n;
        } else if (n < 0) {
            return v >> n;
        } else {
            return v;
        }
    }

    // TYPE CONVERSION
    int _ORD(char v) {
        return (int) v;
    }

    char _CHR(int v) {
        return (char) v;
    }

    // PROCEDURES
    void _INC(int &v, int x = 1) {
        v += x;
    }

    void _DEC(int &v, int x = 1) {
        v -= x;
    }

}
