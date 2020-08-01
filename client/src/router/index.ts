import Vue from "vue";
import VueRouter, { Route, RouteConfig } from "vue-router";
import Home from "../views/Home.vue";
import Enroll from "../views/Enroll.vue";
import Contact from "../views/Contact.vue";
import About from "../views/About.vue";
import SignIn from "../views/SignIn.vue";
import ApiService from "@/services/api";
import store from "../store";

Vue.use(VueRouter);
const routes: Array<RouteConfig> = [
    {
      path: "/home",
      redirect: _to => {
        const classId = store.getters.currentClassId;
        return { name: "Home", params: { classId } } ;
      }
    },
    {
      path: "/home/:classId",
      name: "Home",
      component: Home
    },
    {
      path: "/login",
      name: "Login",
      component: SignIn
    },
    {
      path: "/enroll",
      name: "Enroll",
      component: Enroll
    },
    {
      path: "/contact",
      name: "Contact",
      component: Contact
    },
    {
      path: "/about",
      name: "About",
      component: About
    }
  ]

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes 
});

function redirectLogin(to: Route, from: Route) {
  const redirect =
    from.name != "Login" && to.name != "Login" && !ApiService.isSignedIn();
  console.log("redirectLogin says", redirect);
  return redirect;
}

router.beforeEach((to, from, next) => {
  if (redirectLogin(to, from)) {
    next({ name: "Login" });
  } else {
    store.dispatch("syncSessionUserData");
    next();
  }
});

export default router;
