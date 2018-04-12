package filters

import javax.inject.Inject

import play.api.http.DefaultHttpFilters

/**
  * Created by yz on 2017/6/13.
  */

class Filters @Inject()(login: LoginFilter) extends DefaultHttpFilters(login)
