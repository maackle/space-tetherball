package tetherball.things

import skitch.gfx.Sprite
import skitch.core.{Event, EventSink}
import tetherball.Tetherball
import skitch.core.KeyHold

case class FourDirections(left:Int, right:Int, down:Int, up:Int)

class Player(controls:FourDirections) extends Sprite(Tetherball.loader.image("img/player.png")) with EventSink {

	val app = Tetherball

	val speed = 4

	val movementControls = PartialFunction[Event, Unit] {
		case KeyHold(controls.left) => position.x -= speed
		case KeyHold(controls.right) => position.x += speed
		case KeyHold(controls.down) => position.y -= speed
		case KeyHold(controls.up) => position.y += speed
	}

	listen {
		movementControls
	}
}
