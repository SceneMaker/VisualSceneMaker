package de.dfki.vsm;

import org.junit.*;
import org.junit.jupiter.api.Assertions;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.io.File;
import java.lang.System;
import java.util.Arrays;
import java.util.Collection;

import static org.junit.Assert.*;


public class SamplesRunTest {

    @Test
    public void RunSampleConcurrency() throws InterruptedException {
        File file = new File("samples/SampleConcurrency");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleConditionalEdge() throws InterruptedException {
        File file = new File("samples/SampleConditionalEdge");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleConditions2() throws InterruptedException {
        File file = new File("samples/SampleConditions2");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleEpsilonEdge() throws InterruptedException {
        File file = new File("samples/SampleEpsilonEdge");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleForkEdges() throws InterruptedException {
        File file = new File("samples/SampleForkEdges");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleHelloWorld() throws InterruptedException {
        File file = new File("samples/HelloWorld");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleHelloWorld2() throws InterruptedException {
        File file = new File("samples/SampleHelloWorld2");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleIfElse() throws InterruptedException {
        File file = new File("samples/SampleIfElse");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleIfElse2() throws InterruptedException {
        File file = new File("samples/SampleIfElse2");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleIfElse3() throws InterruptedException {
        File file = new File("samples/SampleIfElse3");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleInterruptive() throws InterruptedException {
        File file = new File("samples/SampleInterruptive");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleInterruptiveEdges() throws InterruptedException {
        File file = new File("samples/SampleInterruptiveEdges");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleLoops() throws InterruptedException {
        File file = new File("samples/SampleLoops");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Test
    public void RunSampleTimeoutEdge() throws InterruptedException {
        File file = new File("samples/SampleTimeoutEdge");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /*

    @Test
    public void RunSampleWillFail() throws InterruptedException {
        File file = new File("samples/SampleWillFail");
        Thread thread = new Thread() {
            @Override
            public void run() {
                Core.runtime(file);
            }
        };
        try {
            thread.start();
            Thread.sleep(5000);
            thread.interrupt();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
     */

}