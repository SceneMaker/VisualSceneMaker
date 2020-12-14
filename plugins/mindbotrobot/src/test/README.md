# How to Run the ROSJava Code to connect with the Mindbot


* First you need to launch the Mindbot:
    * Go inside the folder where your mindbot workspace is locate
    * Run the following commands:
      
        catkin_build
      
        source devel/setup.bash
      
        roslaunch mindbot mindbot.launch
* Then open another console
* Go inside the folder 'MindbotCommunication'
* Run the following commands: (you can also add the first two commands in your 'gedit ~/.bashrc' at the end of the file, then you do not need to run these everytime you edit your Publisher/Subscriber)

  source /opt/ros/melodic/setup.bash

  source ~/rosjava/devel/setup.bash
  
  catkin_make
    
  source devel/setup.bash 
  
  cd src/communication_package/communication_project/
    
  ../gradlew install
* Now you have set up everything you need to either run a Publisher or a Listener
* To run the Publisher, run the command:
  
    rosrun communication_package communication_project communication_package.communication_project.MindbotPublisher
* To run the Subscriber, run the command:
    
    rosrun communication_package communication_project communication_package.communication_project.MindbotSubscriber
    