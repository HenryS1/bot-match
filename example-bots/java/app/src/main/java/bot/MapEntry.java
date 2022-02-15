package bot;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type")
@JsonSubTypes({
    @Type(value = Rock.class, name = "Rock"),
    @Type(value = Assassin.class, name = "Assassin"),
    @Type(value = Scout.class, name = "Scout"),
    @Type(value = Tank.class, name = "Tank"),
    @Type(value = Tank.class, name = "Base") })
public abstract class MapEntry {
    public String type;
    public Coord position;
}
