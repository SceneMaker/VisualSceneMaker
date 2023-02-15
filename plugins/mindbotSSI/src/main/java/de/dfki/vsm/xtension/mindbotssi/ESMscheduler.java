package de.dfki.vsm.xtension.mindbotssi;

/*
For information on time management see:
* An official introduction on the new Java8 java.time package:
  - https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
* Another comprehensive overview, with some more examples on Period and Duration
  - https://www.baeldung.com/java-8-date-time-intro
* An interesting answer on Stackoverflow on time comparison:
  - https://stackoverflow.com/a/60579878/2010713
* DateTimeFormatter docs (useful for parsing):
  - https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
*/

import java.io.*;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.LinkedList;
import java.util.ListIterator;

public class ESMscheduler {

    /** The list of time entries of he schedule. */
    private final LinkedList<LocalDateTime> _schedule = new LinkedList() ;

    /** Iterator pointing to the element that we are currently waiting for.
     * If the whole schedule has been consumed, the iterator will be pointing at the end of the line, and hasNext() is false.
     */
    private ListIterator<LocalDateTime> _currentWaitingIterator;
    /** The current waiting entry. Might be null. */
    private LocalDateTime _currentWaitingEntry;

    public ESMscheduler(File schedule_file) throws IOException {

        _parseSchedule(schedule_file);

    }

    private void _parseSchedule(File f) throws IOException {

        BufferedReader reader = new BufferedReader(new FileReader(f)) ;

        String header = reader.readLine();
        // System.out.println("Header: "+header);

        String line ;
        while((line=reader.readLine()) != null) {
            //System.out.println("line: " + line) ;

            LocalDateTime ldt = LocalDateTime.parse(line, DateTimeFormatter.ofPattern("dd.MM.yyyy,H:mm"));
            System.out.println("Parsed: "+ldt) ;
            _schedule.add(ldt) ;
        }

        _currentWaitingIterator = _schedule.listIterator();
        if(_currentWaitingIterator.hasNext()) {
            _currentWaitingEntry = _currentWaitingIterator.next();
        }

    }


    /** Returns the entry in which the scheduler is waiting for.
     * @return
     */
    public LocalDateTime waitingFor() {
        return _currentWaitingEntry ;
    }

    /** Move the iterator through the schedule until all elements older than "now" have been discaded.
     */
    public void skipOldEntries() {
        LocalDateTime now = LocalDateTime.now() ;
        System.out.println("Comparing with " + now) ;

        if(_currentWaitingEntry == null) {
            return ;
        }

        while(_currentWaitingEntry.isBefore(now)) {
            System.out.println("Skipping entry " + _currentWaitingEntry) ;

            if(_currentWaitingIterator.hasNext()) {
                _currentWaitingEntry = _currentWaitingIterator.next();
            } else {
                _currentWaitingEntry = null ;
                break ;
            }
        }
    }

    /** Checks weather the input time (which is supposed to be the current time) has reached the current waiting entry.
     * If yes, true is returned and the "waitingFor()" entry is updated to the next entry in the scheduler.
     * Otherwise, false is returned and no iternal fields are updated.
     * If the current schedule entry is null (the schedule is exhausted) returns false.
     *
     * @return true if the time has been reached by the current time (and the next waiting entry is updated), or false otherwise.
     */
    public boolean nextItemReached() {
        LocalDateTime now = LocalDateTime.now() ;

        if (_currentWaitingEntry == null) {
            return false ;
        }

        if(_currentWaitingEntry.isBefore(now)) {

            // updated the iterator and the current entry.
            if(_currentWaitingIterator.hasNext()) {
                _currentWaitingEntry = _currentWaitingIterator.next();
            } else {
                _currentWaitingEntry = null;
            }

            return true ;

        } else {
            // current entry not reached, yet
            return false ;
        }


    }

    /** A static main method to test the functionalities fo this class.
     *
     * @param argv
     * @throws IOException
     * @throws InterruptedException
     */
    public static void main(String[] argv) throws IOException, InterruptedException {

        ESMscheduler es = new ESMscheduler(new File("ESM_schedule.csv")) ;
        System.out.println("") ;

        System.out.println("Current scheduled item: " + es._currentWaitingEntry) ;

        System.out.println("") ;

        System.out.println("Skipping old entries") ;
        es.skipOldEntries();

        System.out.println("Current schedule item: " + es.waitingFor()) ;

        // cycle until a new entry is reached
        while(true) {
            System.out.println("Still waiting for entry " + es.waitingFor()) ;
            Thread.sleep(1000);
            if(es.nextItemReached()) {
                System.out.println("Reached!!!") ;
                break;      // <-- CYCLE BREAK !!!
            }
        }

        System.out.println("Current schedule item: " + es.waitingFor()) ;

        System.out.println("All done.");
    }

}


