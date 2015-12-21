/**
 * Created by duy on 09/10/2015.
 */


import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;


import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import twitter4j.JSONObject;

public class TweetManager {
    private static String getURLResponse(String from, String since, String until, String querySearch, String scrollCursor) throws Exception {
        String appendQuery = "";
        if (from != null) {
            appendQuery += "from:" + from;
        }
        if (since != null) {
            appendQuery += " since:" + since;
        }
        if (until != null) {
            appendQuery += " until:" + until;
        }
        if (querySearch != null) {
            appendQuery += " " + querySearch;
        }

        String url = String.format("https://twitter.com/i/search/timeline?f=realtime&q=%s&src=typd&max_position=%s", URLEncoder.encode(appendQuery, "UTF-8"), scrollCursor);

        URL obj = new URL(url);
        HttpURLConnection con = (HttpURLConnection) obj.openConnection();

        con.setRequestMethod("GET");

        BufferedReader in = new BufferedReader(
                new InputStreamReader(con.getInputStream()));
        String inputLine;
        StringBuffer response = new StringBuffer();

        while ((inputLine = in.readLine()) != null) {
            response.append(inputLine);
        }
        in.close();

        return response.toString();
    }

    /**
     * @param username (without @)
     * @param since    Lower bound date (yyyy-mm-dd)
     * @param until    Upper bound date (yyyy-mm-dd)
     * @return A list of all tweets found
     */
    public static List<Tweet> getTweets(String username, String since, String until, String querySearch, int limit) {
        List<Tweet> results = new ArrayList<Tweet>();

        try {
            String refreshCursor = null;
//            for (int i = 0; i < limit; ++i) {
            int count = 0;
            while(true) {
//                System.out.println("fetching " + i + " out of " + limit);
                System.out.println("fetching " + ++count);
                JSONObject json = new JSONObject(getURLResponse(username, since, until, querySearch, refreshCursor));
                refreshCursor = json.getString("min_position");
                Document doc = Jsoup.parse((String) json.get("items_html"));
                Elements tweets = doc.select("div.js-stream-tweet");

                if (tweets.size() == 0) {
                    break;
                }

                for (Element tweet : tweets) {
                    String usernameTweet = tweet.select("span.username.js-action-profile-name b").text();
                    String txt = tweet.select("p.js-tweet-text").text().replaceAll("[^\\u0000-\\uFFFF]", "");
                    int retweets = Integer.valueOf(tweet.select("span.ProfileTweet-action--retweet span.ProfileTweet-actionCount").attr("data-tweet-stat-count").replaceAll(",", ""));
                    int favorites = Integer.valueOf(tweet.select("span.ProfileTweet-action--favorite span.ProfileTweet-actionCount").attr("data-tweet-stat-count").replaceAll(",", ""));
                    long dateMs = Long.valueOf(tweet.select("small.time span.js-short-timestamp").attr("data-time-ms"));
                    Date date = new Date(dateMs);

                    Tweet t = new Tweet(usernameTweet, txt, date, retweets, favorites);
                    results.add(t);
                }
            }
            System.out.println("exit loops");
        } catch (Exception e) {
            System.out.println("exit");
            e.printStackTrace();
        }

        return results;
    }

    public static void exportToCSV(List<Tweet> tweets, String separator) {
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("tweets.csv"), "utf-8"));
            writer.write("Date" + separator + "User" + separator + "Retweets" + separator + "Favourites" + separator + "Tweet" + "\n");
            for (Tweet tweet : tweets) {
                System.out.println("*******************");
                System.out.println(tweet.getDate());
                System.out.println(tweet.getUsername());
                System.out.println(tweet.getRetweets());
                System.out.println(tweet.getFavorites());
                System.out.println(tweet.getText());

                writer.write(tweet.getDate() + separator
                        + tweet.getUsername() + separator
                        + tweet.getRetweets() + separator
                        + tweet.getFavorites() + separator
                        + tweet.getText() + "\n");
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
