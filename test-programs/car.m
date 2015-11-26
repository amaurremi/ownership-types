class Engine {
    void start () {}
    void stop () {}
}

class Driver {}

class Car {
    rep Engine engine;      // representation
    norep Driver driver;    // not representation

    Car() {
        engine = new rep Engine();
        driver = null;
    }

    rep Engine getEngine() {
        // return engine;
    }

    void setEngine(rep Engine e) {
        engine = e;
    }

    void go() {
        // if (driver != null) engine.start();
    }
}

class Main {
    void main() {
        norep Driver bob = new norep Driver();
        norep Car car = new norep Car();
        rep Engine e;

        car.driver = bob;
        car.go();
        // car.engine.stop();           // fails
        // car.getEngine().stop();      // fails

        e = new rep Engine();
        // car.setEngine(e);            // fails
    }
}