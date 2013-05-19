package tetherball.things

import tetherball.Tetherball.Thing
import skitch.stage.box2d.{Embodied, B2Implicits, ManagedEmbodied}
import skitch.vector.{vec, vec2}
import org.jbox2d.dynamics.{Body, Filter, BodyType, World}
import skitch._
import skitch.core.components.CircleShape
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.joints._
import skitch.helpers
import skitch.gfx
import Types._
import skitch.gfx.Font
import skitch.core.SkitchApp
import org.lwjgl.opengl.GL11
import tetherball.Tetherball
import tetherball.Tetherball.Bits._
import scala.Some
import skitch.helpers.Radian
import skitch.Types.Radian

class FrameCached[A](val calc:()=>A)(implicit app:SkitchApp) {
	private var lastFrame = -1
	private var cache:A = _

	def apply():A = {
		if (app.ticks > lastFrame) {
			cache = calc()
			lastFrame = app.ticks
		}
		cache
	}
}

object FrameCached {

	implicit def framecached2val[A](fc:FrameCached[A]) = fc()

	def apply[A](calc: =>A)(implicit app:SkitchApp) = new FrameCached( () => calc)(app)
}

class Rope(numNodes:Int, pole:Pole, val ball:Ball)(implicit world:World, app:SkitchApp) extends Thing with B2Implicits { rope =>

	import Rope._

	val slackFactor = 1f

	private val direction = (ball.position - pole.position).unit

	object poleNode extends Node( pole.position + direction * (pole.radius + Rope.radius), 0 ) {
		override def isTouching = true
	}
	object ballNode extends Node( ball.position - direction * (ball.radius + Rope.radius), numNodes-1 )

	lazy val setup = {

//
//		val poleNode = new Node( pole.position + direction * (pole.radius + Rope.radius), 0 )
//		val ballNode = new Node( ball.position - direction * (ball.radius + Rope.radius), numNodes-1 )

		val numInnerNodes = numNodes - 2
		val innerNodes = for(i <- List.range(1, numInnerNodes+1)) yield {
			val t = i.toFloat / (numNodes-1)
			new Node( vec.lerp(poleNode.position, ballNode.position, t), i )
		}

		val allNodes = (poleNode :: (ballNode :: innerNodes.reverse).reverse).toIndexedSeq

		val poleWeld = Rope.Joint.weld(pole.body, poleNode.body, pole.position + direction * pole.radius)
		val ballWeld = Rope.Joint.weld(ball.body, ballNode.body, ball.position - direction * ball.radius)

		val attachmentAngle = (poleNode.position - pole.position).angle

		// a moving dummy node from which all `HardLimit`s are based
		val contactNode = new Node(poleNode.position, -1)
		contactNode.body.setType(BodyType.KINEMATIC)

		(innerNodes, allNodes, contactNode, attachmentAngle)
	}

	val (innerNodes, allNodes, contactNode, attachmentAngle) = setup

	val initialLength = (ballNode.position - poleNode.position).length
	val maxLength = initialLength * 1.1f
	val initialSpacing = initialLength / (numNodes - 1)

	val nodePairs = for((a,b) <- helpers.pairs(allNodes)) yield NodePair(a, b)

	val hardLimits = (ballNode :: innerNodes).map(new HardLimit(_))

	lazy val font = new Font("font/UbuntuMono-R.ttf", 24)

	val history = helpers.MemFloat(500)

	val interNodeWrappedAngle = {
		val R = Rope.radius + pole.radius
		val L = initialSpacing
		math.acos( (2*R*R - L*L) / (2*R*R) )
	}

	val wrappedChain = FrameCached {
		allNodes.takeWhile { n =>
			n.isTouching
		}
	}

	val wrappedChainPairs = FrameCached {
		if(wrappedChain.isEmpty) IndexedSeq.empty
		else nodePairs.take(wrappedChain.last.ordinal)
	}

	def windingAngle:Radian = {
		ballNode.windAngle
	}

	def windingDirection = math.signum(windingAngle)

	def woundLength:Real = {
		val r = pole.radius + Rope.radius
		math.abs(contactAngle.toFloat * r)
	}

	val realLength = FrameCached {
		(for( p <- nodePairs ) yield {
			p.distance()
		}).sum
	}

	def error = {
		(for( p <- nodePairs ) yield {
			math.pow(math.max(p.distance() - initialSpacing, 0), 2).toFloat
		}).sum
	}

	def contactAngle: Radian = {
		val wa = windingAngle
		val halfPi = math.Pi / 2

		if (wa > halfPi) wa - halfPi
		else if (wa < -halfPi) wa + halfPi
		else 0.0
	}

	def contactPoint: vec2 = vec.polar(pole.radius + Rope.radius, contactAngle + attachmentAngle)

	def update(dt:Float) {
		contactNode.body.setTransform(contactPoint, 0)
		allNodes.foreach(_.update(dt))
		hardLimits.foreach(_.update(dt))
		nodePairs.foreach(_.update(dt))

	}

	def render() {
		Color(0xff00ff).bind()
		gfx.circle(0.1f, contactPoint)

		Color.white.bind()
		gl.lineWidth(2f)
		gl.begin(GL11.GL_LINE_STRIP) {
			allNodes.foreach(n => gl.vertex(n.position))
		}
		gl.lineWidth(1f)

		allNodes.foreach(_.render())
		hardLimits.foreach(_.render())

		history << windingAngle.toFloat

		gl.matrix {
			gl.scale(1/50f, 0.5f)
			gl.begin(GL11.GL_LINE_STRIP) {
				for ((y, t) <- history.mem.zipWithIndex) {
					gl.vertex(t, y + 2)
				}
			}
		}
	}

	class Node(initialPosition:vec2, val ordinal:Int)(implicit val world:World) extends ManagedEmbodied with CircleShape {

		val radius = Rope.radius
		private[Rope] var _touching = false
		private[Rope] var _jointA, _jointB: Option[DistanceJoint] = None
		private[Rope] var _windAngle = 0.0

		def maxWindAngle = rope.interNodeWrappedAngle * ordinal

		def jointA = _jointA
		def jointB = _jointB
		def windAngle = _windAngle

		def setTouching(on:Boolean) {
			_touching = on
		}

		def isTouching = _touching

		// essentially, is this node "behind" the contactNode?  If so we don't have to "rein it in"
		def isBehindContactNode = {
			val sign = math.signum(rope.contactAngle)
			windAngle * sign < rope.contactAngle * sign
		}

		override def update(dt:Float) {
			if (isTouching) {
				val normal = (position - rope.pole.position).unit
				val tangent = normal.rotate(math.Pi/2 * windingDirection)
				if(velocity.dot(normal) < 0) {
					// remove velocity component going into the pole (to discourage from going *through* it)
					body.setLinearVelocity( velocity.project(tangent) )
				}
				if(math.abs(windAngle) > maxWindAngle && velocity.dot(tangent) > 0) {
					// if already wrapped too far, remove velocity component which would cause further wrapping
					body.setLinearVelocity( velocity.project(normal) )
				}
				// this used to be an attempt to modify the position directly -- totally unstable.
				if (isTouching && math.abs(windAngle) > maxWindAngle) {
					val ideal = vec.polar(Rope.radius + rope.pole.radius, rope.windingDirection * maxWindAngle + poleNode.position.angle)

				}
			}
		}

		def render() {
			if (isBehindContactNode && math.abs(windAngle) > maxWindAngle*0.9) {
				gl.fill(true)
				Color.cyan.bind()
			}
			else {
				gl.fill(false)
				Color.yellow.bind()
			}
			gfx.circle(radius, position)
			Color.black.bind()
			gfx.vector(position, velocity / 50)
		}

		lazy val body = {
			val fixture = Embodied.defaults.fixtureDef
			val bodydef = Embodied.defaults.bodyDef

			bodydef.`type` = BodyType.DYNAMIC
			bodydef.position = initialPosition
			val body = world.createBody(bodydef)

			val filter = new Filter
			filter.categoryBits = ROPE_NODE_BIT
			filter.maskBits = POLE_BIT

			val circle = new shapes.CircleShape()
			circle.m_radius = this.radius

			fixture.shape = circle
			fixture.restitution = 0f
			fixture.density = 1f
			fixture.userData = this
			fixture.filter = filter

			body.createFixture(fixture)
			body
		}
	}

	case class NodePair(a:Node, b:Node) {

		val pair = (a,b)

		val stick = Rope.Joint.stick(a, b, slackFactor)

		val distance = FrameCached {
			(b.position - a.position).length
		}

		def update(dt:Float) {
			val s = (a.position - pole.position).angle
			val t = (b.position - pole.position).angle
			b._windAngle = a._windAngle + helpers.Radian.diff(t, s)

			val dot = (a.velocity dot b.velocity)
			val diff = (b.position - a.position)
			val projA = a.velocity.project(diff)
			val projB = b.velocity.project(diff)
			// an attempt to never allow divergent velocity near the pole.  TODO: make it happen.
			if ((a.isTouching || b.isTouching) && diff.length > rope.initialSpacing) {
//				b.velocity = b.velocity - projB + projA
			}
		}

	}


	class HardLimit(val node:Node) {

		val earlyTriggerRatio = 0.95f
		val initialLength = (contactNode.body.getPosition - node.body.getPosition).length
		val baseFreq = 20f
		val eps = 1e-5f

		// the joint
		val j = {
			val jd = new DistanceJointDef
			jd.initialize(contactNode.body, node.body, contactNode.body.getPosition, node.body.getPosition)
			jd.length = initialLength
			jd.frequencyHz = Rope.Joint.freqHz
			jd.dampingRatio = Rope.Joint.damping
			world.createJoint(jd).asInstanceOf[DistanceJoint]
		}

		// speed in the direction pointing out from the pole
		def outwardSpeed: Real = {
			( (node.position - contactNode.position).unit dot (node.velocity) )
		}

		// real distance from contactNode to this node
		val actualLength = FrameCached {
			(node.position - contactNode.position).length
		}

		// length below which the limit is never activated
		val minLength:Real = {
			node match {
				case `ballNode` => 0f
				case _ => rope.pole.radius*4
			}
		}

		// length at which the limit becomes active
		val maxLength = {
			FrameCached {
				val compensation = wrappedChainPairs.map( p => math.max(0, p.distance - rope.initialSpacing) ).sum
				val total = initialLength + compensation
				math.max(0, total - rope.woundLength)
			}
		}

		val disabledFreq = eps

		def activeFreq = {
			if(actualLength < minLength*2) baseFreq// * (actualLength / minLength*2)
			else baseFreq
		}

		def isActive = {
			val strictly = actualLength > minLength && ! node.isBehindContactNode
			val also = (actualLength > maxLength && outwardSpeed > 0) || (actualLength > maxLength * 1.1f)
			strictly && also
		}

		def update(dt:Float) {
			val diff = (node.position - contactNode.position)
			val over = diff.length / maxLength
			val impulse = diff.unit * (-10000*dt * over)
			if (over > 1) {
//				node.body.applyForceToCenter(impulse)
			}


			if (isActive) {
				j.setLength(maxLength)
				j.setFrequency(activeFreq)
			}
			else {
				j.setLength(maxLength * earlyTriggerRatio)
				j.setFrequency(disabledFreq)
			}
		}

		def render() {
			val diff = (node.position - contactNode.position)

			if (j.getFrequency > eps) Color.blue.bind()
			else Color.gray.bind()
			gfx.vector(contactNode.body.getPosition, diff.unit * maxLength)

		}
	}

}

object Rope {

	val radius = 0.075f

	object Joint extends B2Implicits {
		val freqHz = 30f
		val damping = 2f

		def stick(a:Rope#Node, b:Rope#Node, slackFactor:Float)(implicit world:World):DistanceJoint = {

			val jd = new DistanceJointDef
			jd.initialize(a.body, b.body, a.body.getPosition, b.body.getPosition)
			jd.length = (b.position - a.position).length * slackFactor
			jd.frequencyHz = Joint.freqHz
			jd.dampingRatio = Joint.damping

			val j = world.createJoint(jd).asInstanceOf[DistanceJoint]
			a._jointB = Some(j)
			b._jointA = Some(j)
			j
		}

		def weld(a:Body, b:Body, pos:vec2)(implicit world:World) = {
			val jw = new WeldJointDef
			jw.initialize(a, b, pos)

			world.createJoint(jw)
		}
	}

}
