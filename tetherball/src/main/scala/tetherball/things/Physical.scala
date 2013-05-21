package tetherball.things

import tetherball.TetherballGame.Thing
import skitch.stage.box2d.{Embodied, ManagedEmbodied}
import skitch.core.components.Position2D


trait Physical extends Thing with Embodied with Position2D  {

	def takePuff(from:Player) {
		val diff = position - from.position
		val impulse = diff.unit * from.Puff.power
		body.applyLinearImpulse(impulse, position)
	}
}
