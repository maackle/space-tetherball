package tetherball.states

import skitch.core.managed.{View2D, SkitchState}
import tetherball.Tetherball
import skitch.gfx.Sprite
import skitch.Color
import tetherball.things.{FourDirections, Player}
import skitch.core.KeyDown

class PlayState extends Tetherball.State {

	def onEnter = ()
	def onExit = ()

	val backgroundColor = Some(Color(0x222222))

	val guy = new Player(FourDirections(KEY_LEFT, KEY_RIGHT, KEY_DOWN, KEY_UP))

	val things = Set(guy)

	val views = Seq(
		View2D(things)
	)

	listenTo(guy)

}
