import { UserData, User, Buffer, AuthInfo } from "@/types";

import axios, { AxiosRequestConfig } from "axios";

// import _ from "lodash";

const API_URL = "/api";

const serverResponse = { data: null };

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

class ApiService {
  constructor(private currentUser: User | null) {}
  
  // Auth
  async signIn(info: AuthInfo): Promise<UserData> {
    await delay();
    const response = await axios.post(`${API_URL}/signin`, {
      emailAddress: info.emailAddress,
      password: info.password
    });
    console.log("ApiServer.signIn", serverResponse);
    serverResponse.data = response.data;
    this.currentUser = response.data.user;
    return response.data;
  }

  isSignedIn() {
    return this.currentUser !== null;
  }

}

// const accessToken = localStorage.getItem("accessToken");
// export default new ApiService(accessToken);
export default new ApiService(null);
