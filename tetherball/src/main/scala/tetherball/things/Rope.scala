package tetherball.things

import tetherball.Tetherball.Thing
import skitch.stage.box2d.{B2Implicits, Embodied}
import skitch.vector.{vec, vec2}
import org.jbox2d.dynamics.{Body, Filter, BodyType, World}
import skitch.{helpers, gfx}
import skitch.core.components.CircleShape
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.joints.{Joint, WeldJointDef, DistanceJointDef}

class Rope(numNodes:Int, pole:Pole, val ball:Ball)(implicit world:World) extends Thing {

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
			Rope.Joint.stick(a, b, 1.1f)
		}

		allNodes.map(_.position).foreach(println)
		helpers.pairs(allNodes).map(_._2.position).foreach(println)
		stickJoints.map(j => (j.getBodyA.getPosition, j.getBodyB.getPosition)).foreach(println)

		val poleWeld = Rope.Joint.weld(pole.body, poleNode.body, pole.position + direction * pole.radius)
		val ballWeld = Rope.Joint.weld(ball.body, ballNode.body, ball.position - direction * ball.radius)

		(poleNode, ballNode, innerNodes, allNodes)
	}

	val (poleNode, ballNode, innerNodes, allNodes) = setup

	def update(dt:Float) {

	}

	def render() {

	}

	def build() {

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
			val jw = new WeldJointDef
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
