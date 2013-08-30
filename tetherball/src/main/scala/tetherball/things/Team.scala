package tetherball.things

import tetherball.Winding
import skitch.Color

case class Team(direction:Winding.Value, players:Seq[Player], color:Color) {

}
