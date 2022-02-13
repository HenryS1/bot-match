package bot;

import java.util.Map;
import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;

public class Bot {

    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Input input = mapper.readValue(System.in, Input.class);
            while (System.in.available() > 0) {
                input = mapper.readValue(System.in, Input.class);
            }

            System.out.println("READY");

            Player me;
            Player enemy;
            if (input.you == "player1") {
                me = input.player1;
                enemy = input.player2;
            } else {
                me = input.player2;
                enemy = input.player1;
            }
        
            if (me.money > 10) {
                Coord myBaseCoord = me.base;
                Coord enemyBaseCoord = enemy.base;
                System.out.println(String.format("BUILD SCOUT (%d, %d) (%d, %d)", 
                                                 myBaseCoord.x, myBaseCoord.y + 1,
                                                 enemyBaseCoord.x, enemyBaseCoord.y));
            } else {
                System.out.println("NO-OP");
            }
        } catch (IOException e) {
            System.out.println("NO-OP");
        }
    }

}
