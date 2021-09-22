package de.dfki.vsm.xtension.mindbotrobot;

import java.net.*;
import java.util.Enumeration;
import java.util.List;

public class NetTools {


    private static int addressBytesToInt(byte[] addr_bytes) {
        int addr_int = ((addr_bytes[0] << 24) & 0xff000000) |
                ((addr_bytes[1] << 16) & 0x00ff0000) |
                ((addr_bytes[2] << 8) & 0x0000ff00) |
                ( addr_bytes[3] & 0x000000ff) ;
        return addr_int;
    }

    /** Container for the results of a interface query.
     * @see #getBestInterfaceFor(Inet4Address)
     */
    public static class NetBindingResult {

        public NetworkInterface intf ;
        public InetAddress addr ;

        public NetBindingResult(NetworkInterface interf, InetAddress address) {
            this.intf = interf ;
            this.addr = address ;
        }

    }

    /** Given a remote IP address, searches through all network interfaces for a local IP that is
     * suited for local binding to support bi-directional communication.
     * The search is done by cycling through all available network interfaces for
     * matching subnet addresses.
     *
     * @param remote_addr A remote address to contact (For now, only IPv4 is supported)
     * @return The pair (interface,local_address), where the local address can be used to bind a Socket for a communication.
     * Returns null if no local interface/address can match the request.
     * @throws SocketException Can be raised if the list of interfaces can not be retrieved.
     */
    public static NetBindingResult getBestInterfaceFor(Inet4Address remote_addr) throws SocketException {
        byte[] remote_addr_bytes = remote_addr.getAddress();
        int remote_addr_int = addressBytesToInt(remote_addr_bytes);

        // Look for a compatible address on all the network interfaces...
        Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
        while(interfaces.hasMoreElements()) {
            NetworkInterface intf = interfaces.nextElement();

            // ... and through all of its addresses
            List<InterfaceAddress> intf_addrs = intf.getInterfaceAddresses();
            for(InterfaceAddress intf_addr : intf_addrs) {
                InetAddress local_addr = intf_addr.getAddress();
                byte[] local_addr_bytes = local_addr.getAddress();

                if(local_addr_bytes.length != remote_addr_bytes.length) {
                    // Likely, IP4 vs. IP6
                    continue;
                }

                int local_addr_int = addressBytesToInt(local_addr_bytes) ;

                // Compare the integer (4 bytes) version of the masked addresses.
                short intf_prefix = intf_addr.getNetworkPrefixLength() ;
                int net_mask = 0xffffffff << (32 - intf_prefix) ;
                int intf_addr_masked = local_addr_int & net_mask ;
                int remote_addr_masked = remote_addr_int & net_mask ;

                // If the two netwrok prefixes match, then we use this address
                if(intf_addr_masked == remote_addr_masked) {
                    return new NetBindingResult(intf, local_addr) ;
                }

            }
        }

        return null ;
    }

}
