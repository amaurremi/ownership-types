class Pair<m, n> {
    X<m> fst;
    Y<n> snd;
}

class Intermediate {
    Pair<rep |   rep, norep> pair1;
    Pair<norep | rep, norep> pair2;

    Pair<rep   | rep, norep> a() {
        return pair1;
    }
    Pair<norep | rep, norep> b() {
            return pair2;
    }
    X<rep> x() {
        return pair1.fst;
    }
    Y<norep> y() {
        return pair1.snd;
    }

    void updateX() {
        pair1.fst = new X<rep>();
    }
}

class Main {
    Intermediate<norep> safe;

    void main() {
        Pair<rep   | rep, norep> a;
        Pair<norep | rep, norep> b;
        X<rep> x;
        Y<norep> y;

//        a = safe.a();   // fails
//        b = safe.b();   // fails
//        x = safe.x();   // fails
        y = safe.y();

        safe.updateX();
    }
}