package de.dfki.vsm.xtension.UnityCommunication;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.*;
import java.net.*;
import java.text.DecimalFormat;

public class UnityVsm   extends ActivityExecutor {
    public UnityVsm(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public void launch() {

    }

    @Override
    public void unload() {

    }

    @Override
    public String marker(long id) {
        return "$" + id;
    }

    @Override
    public void execute(AbstractActivity activity) {
        try {
            DatagramSocket serverSocket = new DatagramSocket(51000);
            byte[] receiveData = new byte[51765];
            String sendString = "polo1";
            byte[] sendData = sendString.getBytes("UTF-8");

            Object port=51000;
            System.out.printf("Listening on udp:%s:%d%n",
                    InetAddress.getLocalHost().getHostAddress(), port);
            DatagramPacket receivePacket = new DatagramPacket(receiveData,
                    receiveData.length);

            while(true)

            {
                serverSocket.receive(receivePacket);
                String sentence = new String( receivePacket.getData(), 0,
                        receivePacket.getLength() );

                int length = sentence.length();


                //here is where the distance values are received here from unity

//                if (length == 8) {
//                    String distance = sentence;
//
//                    Float distance1 = Float.parseFloat(distance);
//
//                    System.out.println( activity.getText() + " " + distance1);
//
//
//                    mProject.setVariable("Distance", distance1);


                // performance values are recieved here from unity


                if ( length > 8) {
                    String dots = sentence;
                    System.out.println(dots);

                                        String[] output = dots.split(",");
                    String part1 = output[0];
                    String part2 = output[1];
                    String part3 = output[2];

                    String[] part11 = part1.split(":");
                    String[] part12 = part2.split(":");
                    String[] part13 = part3.split(":");
                    String dots0 = part11[1];
                    String dots1 = part12[1];
                    String dots2 = part13[1];



                    System.out.println("Reached dots" + activity.getText() + " " + dots0);
                    System.out.println("Elapsed time" + activity.getText() + " " + dots1);
                    System.out.println("Accuracy1" + activity.getText() + " " + dots2);

                    Float covereddistance0 = Float.parseFloat(dots0);
                    Float covereddistance1 = Float.parseFloat(dots1);
                    Float covereddistance2 = Float.parseFloat(dots2);
//                    Float covereddistance3 = Float.parseFloat(dots3);
//                    Float covereddistance4 = Float.parseFloat(dots4);
//                    Float covereddistance5 = Float.parseFloat(dots5);


                    DecimalFormat df = new DecimalFormat("0.0000");
                    float f = covereddistance0;
                    f = Float.parseFloat(df.format(f));
                    System.out.println(f);

                    DecimalFormat df1 = new DecimalFormat("0.0000");
                    float f1 = covereddistance1;
                    f1 = Float.parseFloat(df1.format(f1));
                    System.out.println(f1);

                    DecimalFormat df2 = new DecimalFormat("0.0000");
                    float f2 = covereddistance2;
                    f2 = Float.parseFloat(df2.format(f2));
                    System.out.println(f2);



                    mProject.setVariable("Elapsed_time", f);
                    mProject.setVariable("Covered_Distance", f1);
                    mProject.setVariable("Accuracy1", f2);



                    try {


                        int length1 = new File("/home/rhythm/Downloads/Performance").list().length;

                        if (length1 == 0) {
                            System.out.println("1st session");

                            //  TimeUnit.SECONDS.sleep(20);

                            PrintWriter out1 = new PrintWriter("/home/rhythm/Downloads/Performance/Performance1.txt"); // Step 2
                            System.out.print("does not exits ");
                            out1.println(f2);// Step 3
                            out1.close();  // Step 4



                        } else if (length1 == 1) {
                            System.out.println("1st session");




                            PrintWriter out1 = new PrintWriter("/home/rhythm/Downloads/Performance/Performance2.txt"); // Step 2
                            System.out.print("does not exits ");
                            out1.println(f2);// Step 3
                            out1.close();  // Step 4


//        mProject.setVariable("Elapsed_time", covereddistance3);
//        mProject.setVariable("Covered_Distance", covereddistance4);
//        mProject.setVariable("Accuracy", covereddistance5);

                        } else if (length1 == 2) {
                            System.out.println("1st session");

                            //   TimeUnit.SECONDS.sleep(20);

                            PrintWriter out1 = new PrintWriter("/home/rhythm/Downloads/Performance/Performance3.txt"); // Step 2
                            System.out.print("does not exits ");
                            out1.println(f2);// Step 3
                            out1.close();  // Step 4




                        }



                        {


                            Float performanceone = Float.valueOf(0);
                            Float performancetwo = Float.valueOf(0);
                            Float performancethree = Float.valueOf(0);
                            if (new File("/home/rhythm/Downloads/Performance/Performance1.txt").exists()) {
                                // String expected_value = "Hello, world!";
                                File file = new File("/home/rhythm/Downloads/Performance/Performance1.txt");
                                //   String file ="src/test/resources/fileTest.txt";
                                //String file ="/home/rhythm/Downloads/Performance/Performance1.txt";

                                BufferedReader reader = new BufferedReader(new FileReader(file));
                                String currentLine = reader.readLine();
                                System.out.println(currentLine);
                                reader.close();

                                performanceone = Float.parseFloat(currentLine);
                                System.out.println("float value " + performanceone);

                            }
                            if (new File("/home/rhythm/Downloads/Performance/Performance2.txt").exists()) {

                                File file2 = new File("/home/rhythm/Downloads/Performance/Performance2.txt");

                                BufferedReader reader1 = new BufferedReader(new FileReader(file2));
                                String currentLine1 = reader1.readLine();
                                System.out.println(currentLine1);
                                reader1.close();

                                performancetwo = Float.parseFloat(currentLine1);
                                System.out.println("float value " + performancetwo);

                            }

                            if (new File("/home/rhythm/Downloads/Performance/Performance3.txt").exists()) {
                                File file3 = new File("/home/rhythm/Downloads/Performance/Performance3.txt");

                                BufferedReader reader2 = new BufferedReader(new FileReader(file3));
                                String currentLine2 = reader2.readLine();
                                System.out.println(currentLine2);
                                reader2.close();

                                performancethree = Float.parseFloat(currentLine2);
                                System.out.println("float value " + performancethree);


                            }


                            if ((performanceone<performancetwo) && (performanceone < performancethree)){

                                mProject.setVariable("Comparison", "a decent in your performance.You performed the best in the first session when compared to the second and the third session.");

                            }

                            if ((performancetwo < performanceone) && (performancetwo < performancethree)) {

                            System.out.println("performance two is better" + performancetwo);
                             mProject.setVariable("Comparison", "You performed the best in the second session when compared to the first and the third session");

                            }

                            if ((performancethree < performanceone) && (performancethree < performancetwo)) {

                                System.out.println("performance three is better" + performancethree);
                                mProject.setVariable("Comparison", "a significant improvement across all your sessions. you performed the best in the third session when compared to the first and the second session");
                            }













}

                } catch (IOException e) {
                        throw new RuntimeException(e);
                    } catch (NumberFormatException e) {
                        throw new RuntimeException(e);
                    }


                    DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length,
                        receivePacket.getAddress(), receivePacket.getPort());
                serverSocket.send(sendPacket);
            }
        }
    } catch (IOException e) {
            throw new RuntimeException(e);
        } catch (NumberFormatException e) {
            throw new RuntimeException(e);
        }


}}