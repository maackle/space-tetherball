package tetherball.things

import tetherball.Tetherball.Thing
import skitch.stage.box2d.{B2Implicits, Embodied}
import skitch.vector.{vec, vec2}
import org.jbox2d.dynamics.{Body, Filter, BodyType, World}
import skitch._
import skitch.core.components.CircleShape
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.joints._
import skitch.helpers
import skitch.gfx
import Types._

class Rope(numNodes:Int, pole:Pole, val ball:Ball)(implicit world:World) extends Thing with B2Implicits { rope =>

	import Rope._

	lazy val setup = {
		val pole2ball = ball.position - pole.position
		val direction = pole2ball.unit

		val poleNode = new Rope.Node( pole.position + direction * (pole.radius + Rope.Node.radius) )
		val ballNode = new Rope.Node( ball.position - direction * (ball.radius + Rope.Node.radius) )

		val numInnerNodes = numNodes - 2
		val innerNodes = for(i <- List.range(1, numInnerNodes)) yield {
			val t = i.toFloat / numInnerNodes
			new Rope.Node( vec.lerp(poleNode.position, ballNode.position, t) )
		}

		val allNodes = (poleNode :: (ballNode :: innerNodes.reverse).reverse)

		val stickJoints = for( (a,b) <- helpers.pairs(allNodes)) yield {
			Rope.Joint.stick(a, b, 1.0f)
		}

		val poleWeld = Rope.Joint.weld(pole.body, poleNode.body, pole.position + direction * pole.radius)
		val ballWeld = Rope.Joint.weld(ball.body, ballNode.body, ball.position - direction * ball.radius)

		val attachmentAngle = (poleNode.position - pole.position).angle

		val contactNode = new Rope.Node(poleNode.position) // a dummy node which joins the ball to the pole directly
		contactNode.body.setType(BodyType.KINEMATIC)

		(poleNode, ballNode, innerNodes, allNodes, contactNode, attachmentAngle)
	}

	lazy val (poleNode, ballNode, innerNodes, allNodes, contactNode, attachmentAngle) = setup

//	val grandJoint = new GrandJoint(ballNode)

	def windingAngle:Radian = {
		helpers.pairs(allNodes).map({
			case (a,b) =>
				val s = (a.position - pole.position).angle
				val t = (b.position - pole.position).angle
				helpers.Radian.diff(t, s)
		}).sum
	}

	def woundLength:Real = {
		val r = pole.radius + Rope.Node.radius
		contactAngle.toFloat * r
	}

	def outwardSpeed(body: Body): Real = {
		( (body.getPosition - pole.position).unit dot (body.getLinearVelocity) )
	}

	def contactAngle: Radian = {
		val wa = windingAngle
		val halfPi = math.Pi / 2

		if (wa > halfPi) wa - halfPi
		else if (wa < -halfPi) wa + halfPi
		else 0.0
	}

	def contactPoint: vec2 = vec.polar(pole.radius + Rope.Node.radius, contactAngle + attachmentAngle)

	def update(dt:Float) {
		allNodes.foreach(_.update(dt))
//		grandJoint.update(dt)
	}

	def render() {
		Color(0xff00ff).bind()
		gfx.circle(0.1f, contactPoint)
	}

	class GrandJoint(val node:Rope.Node) {

		val initialLength = (contactNode.body.getPosition - ballNode.body.getPosition).length
		val baseFreq = 30f
		val eps = 1e-10f

		val j = {
			val jd = new DistanceJointDef
			jd.initialize(contactNode.body, node.body, contactNode.body.getPosition, node.body.getPosition)
			jd.length = initialLength
			jd.frequencyHz = Rope.Joint.freqHz
			jd.dampingRatio = Rope.Joint.damping
			world.createJoint(jd).asInstanceOf[DistanceJoint]
		}

		def maxLength(woundLength: Real) = {
			initialLength - woundLength
		}

		def frequency(length:Real):Real = {
			val over = length / initialLength
			println(over)
			if (over >= 1) over * baseFreq
			else eps
			eps
		}

		def update(dt:Float) {
			val len = maxLength(rope.woundLength)
			val freq = {
				if (rope.outwardSpeed(node.body) > 0) frequency(len)
				else eps
			}
			j.setLength(len)
			j.setFrequency(freq)
		}
	}

}

object Rope {

	object Node {
		val radius = 0.1f
	}

	class Node(val position:vec2)(implicit val world:World) extends Embodied with CircleShape {

		val radius = Node.radius

		def render() {
			gfx.circle(radius)
		}

		lazy val body = {
			val fixture = Embodied.defaults.fixtureDef
			val bodydef = Embodied.defaults.bodyDef

			bodydef.`type` = BodyType.DYNAMIC
			bodydef.position = position
			val body = world.createBody(bodydef)

			val filter = new Filter
			filter.categoryBits = 0x04
			filter.maskBits = 0xff - 1

			val circle = new shapes.CircleShape()
			circle.m_radius = this.radius

			fixture.shape = circle
			fixture.restitution = 0f
			fixture.userData = this
			fixture.filter = filter

			body.createFixture(fixture)
			body
		}
	}

	object Joint extends B2Implicits {
		val freqHz = 20f
		val damping = 0.5f

		def stick(a:Node, b:Node, slackFactor:Float)(implicit world:World) = {

			val jd = new DistanceJointDef
			jd.initialize(a.body, b.body, a.body.getPosition, b.body.getPosition)
			jd.length = (b.position - a.position).length * slackFactor
			jd.frequencyHz = Joint.freqHz
			jd.dampingRatio = Joint.damping

			world.createJoint(jd)
		}

		def weld(a:Body, b:Body, pos:vec2)(implicit world:World) = {
			val jw = new WeldJointDef
			jw.initialize(a, b, pos)

			world.createJoint(jw)
		}
	}

}
