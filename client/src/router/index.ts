import Vue from "vue";
import VueRouter, { RouteConfig } from "vue-router";
import store from "../store";
import About from "../views/About.vue";
import Contact from "../views/Contact.vue";
import Home from "../views/Home.vue";
import Reset from "../views/Reset.vue";
import Settings from "../views/Settings.vue";
import SignIn from "../views/SignIn.vue";

Vue.use(VueRouter);
const routes: Array<RouteConfig> = [
  {
    path: "/home",
    redirect: (_to) => {
      const classId = store.getters.currentClassId;
      return { name: "Home", params: { classId } };
    },
  },
  {
    path: "/home/:classId",
    name: "Home",
    component: Home,
  },
  {
    path: "/login",
    name: "Login",
    component: SignIn,
  },
  {
    path: "/settings",
    name: "Settings",
    component: Settings,
  },
  {
    path: "/contact",
    name: "Contact",
    component: Contact,
  },
  {
    path: "/about",
    name: "About",
    component: About,
  },
  {
    path: "/reset",
    name: "Reset",
    component: Reset,
  },
];

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes,
});

router.beforeEach((to, from, next) => {
  if (to.name === "Login" || to.name === "Reset") {
    next();
  } else {
    store
      .dispatch("syncSessionUserData")
      .then(() => next())
      .catch(() => next({ name: "Login" }));
  }
});

export default router;
