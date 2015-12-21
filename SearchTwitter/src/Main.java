import java.io.*;
import java.util.List;

/**
 * Created by duy on 09/10/2015.
 */
public class Main {
    public static void main(String[] args) {
        System.out.println("searching");

        List<Tweet> tweets = TweetManager.getTweets(null, "2015-11-01", "2015-12-13", "david cameron" , 10000);

        TweetManager.exportToCSV(tweets, ";");
    }
}
