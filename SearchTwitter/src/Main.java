import java.io.*;
import java.util.List;

/**
 * Created by duy on 09/10/2015.
 */
public class Main {
    public static void main(String[] args) {
        System.out.println("searching");

        List<Tweet> tweets = TweetManager.getTweets(null, "2015-04-09", "2015-05-07", "nicola sturgeon", 10000);

        TweetManager.exportToCSV(tweets, "\t");
    }
}
