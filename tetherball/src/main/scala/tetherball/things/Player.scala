package tetherball.things

import skitch.gfx.{Image, Sprite}
import skitch.core._
import skitch.Types._
import tetherball.{Countdown, TetherballGame}
import skitch.vector.{vec, vec2}
import tetherball.TetherballGame.Thing
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.{World, Filter, BodyType}
import skitch.core.components.{Position2D, CircleShape}
import skitch.stage.box2d.{Embodied, ManagedEmbodied}
import skitch.core.KeyHold
import tetherball.things.Player.Controls
import tetherball.TetherballGame.CollisionBits._
import skitch.core.KeyHold
import tetherball.things.Player.Controls
import skitch.{gl, gfx, Color}
import skitch.audio.Sound
import tetherball.states.PlayState


object Player {

	case class Controls(left:Int, right:Int, down:Int, up:Int, act:Int)

}

class Player(initialPosition:vec2, controls:Controls)(implicit state:PlayState, world:World) extends Physical with ManagedEmbodied with CircleShape with EventSink { player =>

	val app = TetherballGame

	val thrustMagnitude = 300

	val initialRadius = 1.5f

	var _radius = initialRadius

	def radius = _radius

  private var team:Team = _

  def setTeam(team:Team) { this.team = team }

  def color = team.color

	object Puff {
		val recoveryTime = 2f
		val expansion = 3f
		val timeWindow = 0.2f
		val timer = new Countdown(recoveryTime)(())

		def isActive = timer.elapsed < timeWindow

		def isRecovering = timer.isRunning

		def ratio = (1 - timer.completion)

		def power = {
			val base = 0.5f
			val magnitude = 100f
			magnitude * ( base + (1-base) * ratio )
		}

		def activate() {
			timer.start()
		}

		def update(dt:Float) {
			timer.update(dt)
			if(isRecovering) {
				if(app.ticks % 2 == 0) {
					body.destroyFixture(body.getFixtureList)
					_radius = initialRadius + (expansion - 1) * (ratio)
					body.createFixture(fixtureDef)
				}
			}
		}
	}

	object SFX {
		val hit = new Sound(TetherballGame.loader.ogg("snd/bump.ogg"))
		val ow = new Sound(TetherballGame.loader.ogg("snd/ow.ogg"))
	}

  SFX  // load it right away

	val movementControls = new EventHandler({
		case KeyHold(controls.left)     => body.applyForce(vec(-thrustMagnitude, 0), position)
		case KeyHold(controls.right)    => body.applyForce(vec(+thrustMagnitude, 0), position)
		case KeyHold(controls.down)     => body.applyForce(vec(0, -thrustMagnitude), position)
		case KeyHold(controls.up)       => body.applyForce(vec(0, +thrustMagnitude), position)
		case KeyHold(controls.act) =>
			Puff.activate()
	})

	def doPuff() {

	}

	def render() {
		color.bind()
		gfx.circle(radius)
	}

	override def update(dt:Float) {
		super.update(dt)
		Puff.update(dt)
	}

	listenTo {
		movementControls
	}

	lazy val body = {

		import TetherballGame.CollisionBits._

		val bodydef = Embodied.defaults.bodyDef

		bodydef.`type` = BodyType.DYNAMIC
		bodydef.position = initialPosition

		val body = world.createBody(bodydef)
		body.createFixture(fixtureDef)
		body
	}

	private val _fixtureDef = Embodied.defaults.fixtureDef

	protected def fixtureDef = {
		val circle = new shapes.CircleShape()
		circle.m_radius = this.radius
		_fixtureDef.shape = circle

		val filter = new Filter
		filter.categoryBits = PLAYER_BIT
		filter.maskBits = 0xff & ~ROPE_NODE_BIT

		_fixtureDef.restitution = 0.5f
    _fixtureDef.friction = 0.5f
		_fixtureDef.userData = this
		_fixtureDef.filter = filter

		_fixtureDef
	}

  object OffScreenMarker extends Thing with AutoAffine2D with Update {

    val chevrons = Seq(new Chevron, new Chevron, new Chevron)
    def arenaViewport = state.arena.view.viewportRect
    def HUDViewport = state.HUDView.viewportRect

    def powerEllipse(angle:Radian)(a:Real, b:Real) = {
      val p = 8.0
      require(p % 2 == 0)
      a * b / math.pow( math.pow(b * math.cos(angle), p) + math.pow( a * math.sin(angle), p), 1/p)
    }

    def position = {
      val angle = player.position.angle
      val r = powerEllipse(angle)(HUDViewport.width/2, HUDViewport.height/2) - 2
      val position = vec.polar(r, angle)
      position
    }

    def rotation = player.position.angle

    def scaling = {
      val playerScreen:vec2 = state.view.toScreen(player.position) - state.view.windowRect.center
      val markerScreen = state.HUDView.toScreen(position) - state.view.windowRect.center
      vec2.one * (markerScreen.lengthSquared / playerScreen.lengthSquared)
    }

    class Chevron extends Sprite {
      val app = TetherballGame
      def rotation = player.rotation
      def position = player.position
      val scaling = vec2.one
      val image = TetherballGame.loader.image("img/chevron-right.png") { tex =>
        Image(tex, origin=vec2(23,40), blitColor=player.color)
      }
    }

    def update(dt:Float) {
      for ((chevron, i) <- chevrons.zipWithIndex) {
        val phase = i * 2 * math.Pi / 6
//        chevron.image.is.blitColor = Color.white
      }
    }

    def render() {
      if ( ! arenaViewport.hitTest(player.position) ) { // offscreen
        gl.matrix {
          for ((chevron, i) <- chevrons.zipWithIndex) {
            chevron.render()
            gl.translate(-0.5f, 0)
          }
        }
      }
    }
  }

}
