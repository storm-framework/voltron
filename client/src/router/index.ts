import Vue      from "vue";
import VueRouter, { Route, RouteConfig } from "vue-router";
import Home       from "../views/Home.vue";
import Contact    from "../views/Contact.vue";
// import Instructor from "../views/Instructor.vue";
// import Student    from "../views/Student.vue";
import SignIn     from "../views/SignIn.vue";
import ApiService from "@/services/api";

Vue.use(VueRouter);

const routes: Array<RouteConfig> = [
  {
    path: "/",
    name: "Home",
    component: Home
  },
  {
    path: "/login",
    name: "Login",
    component: SignIn 
  },
  // {
  //   path: "/instructor",
  //   name: "Instructor",
  //   component: Instructor
  // },
  // {
  //   path: "/student",
  //   name: "Student",
  //   component: Student
  // },
  {
    path: "/contact",
    name: "Contact",
    component: Contact
  },
  {
    path: "/about",
    name: "About",
    // route level code-splitting
    // this generates a separate chunk (about.[hash].js) for this route
    // which is lazy-loaded when the route is visited.
    component: () =>
      import(/* webpackChunkName: "about" */ "../views/About.vue")
  }
];

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes
});

function redirectLogin(to:Route, from:Route) {
  const cond = (from.name != "Login" && to.name != "Login" && !ApiService.isSignedIn());
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
