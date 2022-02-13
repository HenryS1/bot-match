package bot;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type")
@JsonSubTypes({
    @Type(value = Rock.class, name = "ROCK"),
    @Type(value = Assassin.class, name = "ASSASSIN"),
    @Type(value = Scout.class, name = "SCOUT"),
    @Type(value = Tank.class, name = "TANK"),
    @Type(value = Tank.class, name = "BASE") })
public abstract class MapEntry {
    public String type;
    public Coord position;
}
