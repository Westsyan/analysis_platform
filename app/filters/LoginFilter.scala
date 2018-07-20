package filters

import javax.inject.Inject

import akka.stream.Materializer
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by yz on 2017/6/13.
  */
class LoginFilter @Inject()(implicit val mat: Materializer, ec: ExecutionContext) extends Filter {

  override def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {
    if ((rh.session.get("id").isEmpty || (rh.session.get("id").get != "1" && rh.session.get("user").isEmpty)) &&
      !rh.path.contains("/back") && !rh.path.contains("/assets/")) {
      Future.successful(Results.Redirect("http://"+ rh.domain +"/").flashing("info" -> "请先登录!"))
    } else {
      f(rh)
    }
  }

}
