import { UserData, User, Buffer, AuthInfo, LoginResponse } from "@/types";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

function mockUser(userName: string): User {
  return { firstName: userName, lastName: "" };
}

const BUFFERS: { [id: string]: Buffer } = {
  0: { id: 0, hash: "-M9Kx-cxRIUgCqVCtjCr", text: "-- Code for group 0\n" },
  1: { id: 1, hash: "-M9L5YBS0kgvUfuz0Ckc", text: "-- Code for group 1\n" },
  2: { id: 2, hash: "-M9L5oPt0fsruy16vntv", text: "-- Code for group 2\n" },
  3: { id: 3, hash: "-M9L5vCVa5FQ0noobA9V", text: "-- Code for group 3\n" },
  4: { id: 4, hash: "-M9L6XICO2mz_yfpDXWR", text: "-- Code for group 4\n" },
  5: { id: 5, hash: "-M9L6nLdsLy_7aXIs4MX", text: "-- Code for group 5\n" }
};
const USERS: { [id: string]: UserData } = {
  rjhala: {
    user: mockUser("rjhala"),
    classes: [
      {
        tag: "Instructor",
        class: "CSE230",
        allBuffers: [BUFFERS[0], BUFFERS[1], BUFFERS[2]]
      },
      {
        tag: "Instructor",
        class: "CSE130",
        allBuffers: [BUFFERS[3], BUFFERS[4], BUFFERS[5]]
      },
      {
        tag: "Student",
        class: "CSE230",
        grpBuffer: BUFFERS[2]
      }
    ]
  },
  nico: {
    user: mockUser("nico"),
    classes: [
      {
        tag: "Student",
        class: "CSE130",
        grpBuffer: BUFFERS[4]
      }
    ]
  },
  rose: {
    user: mockUser("rose"),
    classes: [
      {
        tag: "Student",
        class: "CSE130",
        grpBuffer: BUFFERS[4]
      },
      {
        tag: "Student",
        class: "CSE230",
        grpBuffer: BUFFERS[2]
      }
    ]
  },
  nadia: {
    user: mockUser("nadia"),
    classes: [
      {
        tag: "Instructor",
        class: "CSE130",
        allBuffers: [BUFFERS[3], BUFFERS[4], BUFFERS[5]]
      }
    ]
  }
};

class ApiService {
  constructor(private currentUser: User | null) {}

  getUserData(name: string): UserData {
    return USERS[name];
  }

  getLoginResponse(name: string): LoginResponse {
    return { accessToken: "dummy", user: this.getUserData(name) };
  }

  isSignedIn() {
    return this.currentUser !== null;
  }

  async signIn(info: AuthInfo): Promise<LoginResponse> {
    await delay();
    if (
      info.emailAddress == "rjhala@eng.ucsd.edu" &&
      info.password == "rjhala"
    ) {
      console.log(info, "ok");
      return this.getLoginResponse("rjhala");
    }
    if (
      info.emailAddress == "nlehmann@eng.ucsd.edu" &&
      info.password == "nico"
    ) {
      return this.getLoginResponse("nico");
    }
    if (
      info.emailAddress == "wkunkel@eng.ucsd.edu" &&
      info.password == "rose"
    ) {
      return this.getLoginResponse("rose");
    }
    return Promise.reject("Bad username and/or password!");

    // else if ()
    // this.accessToken = "accessToken";
    // return USERS[SESSION_USER_ID];
  }
}

export default new ApiService(null);
