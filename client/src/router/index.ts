import Vue from "vue";
import VueRouter, { Route } from "vue-router";
import Home from "../views/Home.vue";
import Contact from "../views/Contact.vue";
import About from "../views/About.vue";
import SignIn from "../views/SignIn.vue";
import ApiService from "@/services/api";
import store from "../store";

Vue.use(VueRouter);

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes: [
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
});

function redirectLogin(to: Route, from: Route) {
  const cond =
    from.name != "Login" && to.name != "Login" && !ApiService.isSignedIn();
  console.log("redirectLogin says", cond);
  return cond;
}

router.beforeEach((to, from, next) => {
  if (redirectLogin(to, from)) {
    next({ name: "Login" });
  } else {
    next();
  }
});

export default router;
