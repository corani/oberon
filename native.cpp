extern "C" {
    // FUNTIONS

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

}
