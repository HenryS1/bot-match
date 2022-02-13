package bot;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import java.util.List;

@JsonNaming(PropertyNamingStrategy.KebabCaseStrategy.class)
public class Input {
    public String you;
    public Player player1;
    public Player player2;
}
