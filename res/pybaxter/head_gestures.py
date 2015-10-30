#!/usr/bin/env python
__author__ = 'alvaro'
import socket
import actionlib
import rospy
import struct
import baxter_interface
import baxter_external_devices
from baxter_interface import CHECK_VERSION
from control_msgs.msg import (
    SingleJointPositionAction,
    SingleJointPositionGoal,
)
import thread
import sys, time
from sensor_msgs.msg import (
    Image,
)
import joint_position_file_playback as pl
import cv2
import cv_bridge




class DummyClass(object):
    """
    Class to test TCP/IP communication
    """
    def __init__(self):
        print "Dummy"

    def testDummy(self):
        print "I'm Dummy"

    def bored(self):
        print "I'm bored"

    def smile(self):
        print "I'm smiling"

    def frown(self):
        print "I'm angry"



class HeadGestures(object):
    def __init__(self):



        self._rs = baxter_interface.RobotEnable(CHECK_VERSION)
        self._init_state = self._rs.state().enabled
        print("Enabling robot... ")
        self._rs.enable()
        print("Running. Ctrl-c to quit")
        self._done = False
        self._head = baxter_interface.Head()


    # def clean_shutdown(self):
    #     """
    #     Exits example cleanly by moving head to neutral position and
    #     maintaining start state
    #     """
    #     print("\nExiting example...")
    #     if self._done:
    #         self.set_neutral()
    #     if not self._init_state and self._rs.state().enabled:
    #         print("Disabling robot...")
    #         self._rs.disable()

    def send_image(self,path):
        """
        Send the image located at the specified path to the head
        display on Baxter.

        @param path: path to the image file to load and send
        """
        print "imagen"
        img = cv2.imread(path)
        msg = cv_bridge.CvBridge().cv2_to_imgmsg(img, encoding="bgr8")
        pub = rospy.Publisher('/robot/xdisplay', Image, latch=True, queue_size=1)
        pub.publish(msg)
        # Sleep to allow for image to be published.
        rospy.sleep(1)

    def set_neutral(self):
        """
        Sets the head back into a neutral pose
        """
        self.send_image("res/faces/neutral.jpg")
        self._head.set_pan(0.0)

    def set_nod(self,angle=0):
        self._head.command_nod(angle)

    def negate(self,  p_velocity =50):
        print "Neagting with the head"
        self._head.command_nod(0)
        command_rate = rospy.Rate(10)
        control_rate = rospy.Rate(100)
        start = rospy.get_time()
        counter = -0.61
        go=True
        print self._head.pan()
        while not rospy.is_shutdown() and (rospy.get_time() - start < 6.0):
            angle=counter
            while (not rospy.is_shutdown() and
                   not (abs(self._head.pan() - angle) <=
                       baxter_interface.HEAD_PAN_ANGLE_TOLERANCE)):
                self._head.set_pan(angle, speed=100, timeout=5)



                control_rate.sleep()

            command_rate.sleep()
            if(angle > 0.6 and go):

                go=False

            elif(angle < 0.6 and go):
                counter += 0.7
            if go==False and angle>-0.6:
                counter -= 0.7
            elif go==False and angle<-0.6:
                go=True


        self._done = True



class JointGestures(object):
    def __init__(self):
        self.right = None
        self.left = None

        print("Getting robot state... ")
        rs = baxter_interface.RobotEnable(CHECK_VERSION)
        init_state = rs.state().enabled
        self.right = self.get_right_limb()
        self.left = self.get_left_limb()

        def clean_shutdown():
            print("\nExiting example...")
            if not init_state:
                print("Disabling robot...")
                rs.disable()
        rospy.on_shutdown(clean_shutdown)
        print("Enabling robot... ")
        rs.enable()
        self.move_joint()
        print("Done")

    def get_right_limb(self):
        if self.right is None:
            self.right = baxter_interface.Limb('right')
        return self.right

    def get_left_limb(self):
        if self.left is None:
            self.left = baxter_interface.Limb('left')
        return self.left

    def reset_limb(self, limb_name='right'):
        angles = dict()
        angles['right_s0'] = 0.0
        angles['right_s1'] = 0.0
        angles['right_e0'] = 0.0
        angles['right_e1'] = 0.0
        angles['right_w0'] = 0.0
        angles['right_w1'] = 0.0
        angles['right_w2'] = 0.0

        if limb_name == 'right':
            limb = self.right
        else:
            limb = self.left
        limb.move_to_joint_positions(angles)

    def frange(self, x, y, jump):
      while x < y:
        yield x
        x += jump

    def set_j(self, limb, joint_name, delta):
        current_position = limb.joint_angle(joint_name)
        joint_command = {joint_name: current_position + delta}
        print joint_command
        limb.set_joint_positions(joint_command)

    def move_joint(self):
        left = baxter_interface.Limb('left')
        right = baxter_interface.Limb('right')
        grip_left = baxter_interface.Gripper('left', CHECK_VERSION)
        grip_right = baxter_interface.Gripper('right', CHECK_VERSION)
        lj = left.joint_names()
        rj = right.joint_names()
        for delta in self.frange(0,10,0.1):
            self.set_j(left, 'left_e1', 0.1)

    # funtion to move baxter left's arm and point to the left
    def point_left(self):
        pass

    # funtion to move baxter right's arm and point to the left
    def point_right(self):
        pass




class Gestures(object):
    def __init__(self):
        print("Initializing Head node... ")
        rospy.init_node("vsm_baxter")

        # verify robot is enabled
        print("Getting robot state... ")
        self.head_gestures  = HeadGestures()
        self.joint_gestures = JointGestures()

    # Function to wave
    def wave(self):
        self.moveDefaultPosition()
        wave_1 = {'right_s0': -0.459, 'right_s1': -0.202, 'right_e0': 1.807, 'right_e1': 1.714, 'right_w0': -0.906, 'right_w1': -1.545, 'right_w2': -0.276}
        wave_2 = {'right_s0': -0.395, 'right_s1': -0.202, 'right_e0': 1.831, 'right_e1': 1.981, 'right_w0': -1.979, 'right_w1': -1.100, 'right_w2': -0.448}
        self.joint_gestures.reset_limb()
        limb = self.joint_gestures.get_right_limb()
        for _move in range(3):
            limb.set_joint_position_speed(0.7)
            limb.move_to_joint_positions(wave_1,timeout=1, threshold=0.01)
            limb.move_to_joint_positions(wave_2, timeout=1, threshold=0.01)

    def test(self, *pars):
        print "Test"
        print pars


    # funtion to  make baxter look at specific point
    def lookAt(self):
        pass

    def moveDefaultPosition(self):
        right_wave =  {'right_s0': -0.6070728961120606, 'right_s1': 0.23354857468872073, 'right_w0': 3.0595246779418948, 'right_w1': 1.660917696185303, 'right_w2': 3.0587576875488285, 'right_e0': -0.028378644543457034, 'right_e1': 1.1443496664550783}
        left_wave  =  {'left_s0': -0.6070728961120606, 'left_s1': 0.23354857468872073, 'left_w0': 3.0595246779418948, 'left_w1': 1.660917696185303, 'left_w2': 3.0587576875488285, 'left_e0': -0.028378644543457034, 'left_e1': 1.1443496664550783}
        right = self.joint_gestures.get_right_limb()
        left = self.joint_gestures.get_left_limb()
        left.set_joint_position_speed(0.7)
        right.set_joint_position_speed(0.7)
        right.move_to_joint_positions(right_wave,timeout=1, threshold=0.01)
        left.move_to_joint_positions(left_wave, timeout=1, threshold=0.01)

    # Make Baxter smile :)
    def smile(self):
        print "Smiling"
        self.head_gestures.set_neutral()
        self.head_gestures.send_image("res/faces/smile.jpg")

    #Make Baxter assent
    def assent(self):
        print "Assent"
        self.head_gestures.set_neutral()
        command_rate = rospy.Rate(1)
        for x in range(3):
            print"here"
            self.head_gestures.set_nod(0)
            command_rate.sleep()

    #Make Baxter angry
    def frown(self):
        print "I'm angry"
        self.head_gestures.send_image("res/faces/angry.jpg")
        pass

    def bored(self):
        print "I'm bored"
        self.head_gestures.send_image("res/faces/bored.jpg")

    def negate(self):
        self.head_gestures.negate()

    def handshake(self):
        pl.map_file("gestures/handshake.txt", 1)
        self.moveDefaultPosition

    def come(self):
        pl.map_file("gestures/come.txt", 1)
        self.moveDefaultPosition

    def pointRight(self):
        pl.map_file("gestures/pointright.txt", 1)
        self.moveDefaultPosition


def initHead():
    print("Initializing Head node... ")
    rospy.init_node("rsdk_head_wobbler")
    head_gestures = HeadGestures()
    rospy.on_shutdown(head_gestures.clean_shutdown)
    return head_gestures

def initDummy():
    dummy = DummyClass()
    return dummy

def initJoints():
    joint_gestures = JointGestures()
    return joint_gestures


if __name__ == '__main__':
    #TODO: Separar en una clase que inicialice los nodos
    gestures = initJoints()
    gestures.wave()

    """
    gestures = initHead()
    rospy.on_shutdown(gestures.clean_shutdown)
    gestures.set_neutral()
    gestures.negate()
    """
