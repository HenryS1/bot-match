package bot;

import java.util.Map;
import java.io.IOException;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.DeserializationFeature;

public class Bot {

    public static void main(String[] args) {
        try {
            
            System.out.println("READY");
            while (true) {
                ObjectMapper mapper = new ObjectMapper()
                    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            
                Input input = mapper.readValue(System.in, Input.class);
                while (System.in.available() > 0) {
                    input = mapper.readValue(System.in, Input.class);
                }

                Player me;
                Player enemy;
                if (input.you.equals("player1")) {
                    me = input.player1;
                    enemy = input.player2;
                } else {
                    me = input.player2;
                    enemy = input.player1;
                }

                System.out.println("" + me.money);
                System.out.println(me.team);

                if (me.money > 10) {
                    System.out.println("BUILDING A SCOUT");
                    // Coord myBaseCoord = me.base;
                    // Coord enemyBaseCoord = enemy.base;
                    // System.out.println(String.format("BUILD SCOUT (%d, %d) (%d, %d)", 
                    //                                  myBaseCoord.x, myBaseCoord.y + 1,
                    //                                  enemyBaseCoord.x, enemyBaseCoord.y));
                } else {
                    System.out.println("NO-OP");
                }
            }
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

}
