import { UserData, User, Buffer, AuthInfo } from "@/types";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

const BUFFERS: { [id:string]: Buffer } = {
  0: { id: 0, hash : "-M9Kx-cxRIUgCqVCtjCr", text : "-- Code for group 0\n" },
  1: { id: 1, hash : "-M9L5YBS0kgvUfuz0Ckc", text : "-- Code for group 1\n" },
  2: { id: 2, hash : "-M9L5oPt0fsruy16vntv", text : "-- Code for group 2\n" },
  3: { id: 3, hash : "-M9L5vCVa5FQ0noobA9V", text : "-- Code for group 3\n" },
  4: { id: 4, hash : "-M9L6XICO2mz_yfpDXWR", text : "-- Code for group 4\n" },
  5: { id: 5, hash : "-M9L6nLdsLy_7aXIs4MX", text : "-- Code for group 5\n" },
};

const USERS: { [id:string]: string } = {
  rjhala: "instructor", 
  nico: "0",
  rose: "0",
  deian: "1",
  nadia: "1",
};


class ApiService {
  
  constructor(private currentUser: User | null) {}

  // Auth
  instructor(user: User): UserData {
    return { tag: "Instructor", info: user, allBuffers: Object.values(BUFFERS) }
  }

  student(user: User): UserData { 
    return { tag: "Student", info: user, grpBuffer: BUFFERS[user.group] }
  }

  mockUserData(userName: string): UserData {
    const user: User = { firstName: userName, lastName: "", group: "" } ;
    this.currentUser = user; 
    if (user.firstName == "rjhala") {
      return this.instructor(user);
    } 
    user.group = USERS[user.firstName];
    return this.student(user); 
  }
  
  isSignedIn() {
    return this.currentUser !== null; 
  }

  async signIn(info: AuthInfo): Promise<UserData> {
    await delay();
    if (info.emailAddress == "rjhala@eng.ucsd.edu" && info.password == "rjhala") {
      console.log(info, "ok");
      return this.mockUserData("rjhala");
    };
    if (info.emailAddress == "nlehmann@eng.ucsd.edu" && info.password == "nico") {
      return this.mockUserData("nico");
    }
    if (info.emailAddress == "wkunkel@eng.ucsd.edu" && info.password == "rose") {
      return this.mockUserData("rose");
    }
    return Promise.reject("Bad username and/or password!");

    // else if ()
    // this.accessToken = "accessToken";
    // return USERS[SESSION_USER_ID];
  }

}

export default new ApiService(null);