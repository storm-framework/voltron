import { AuthInfo, ClassLangInfo, EnrollStudent, ResetInfo, ResetPassInfo, Roster, User, UserData } from "@/types";
import axios, { AxiosRequestConfig, AxiosResponse } from "axios";

const API_URL = "/api";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

class ApiService {
  // Auth
  async signIn(info: AuthInfo): Promise<User> {
    // await delay();
    const response: AxiosResponse<User> = await axios.post(
      `${API_URL}/signin`,
      {
        emailAddress: info.emailAddress,
        password: info.password
      }
    );

    return response.data;
  }

  async reset(info: ResetInfo): Promise<string> {
    const response: AxiosResponse<string> = await axios.post(`${API_URL}/reset`, info);
    return response.data;
  }

  async resetPass(info: ResetPassInfo): Promise<string> {
    const response: AxiosResponse<string> = await axios.post(`${API_URL}/resetpass`, info);
    return response.data;
  }

  async signOut(): Promise<void> {
    await axios.post(`${API_URL}/signout`);
  }

  userMe(): Promise<UserData> {
    return this.get("/user/me");
  }

  enroll(students: Roster): Promise<EnrollStudent[]> {
    return this.post(`/enroll`, students);
  }

  setLanguage(info: ClassLangInfo): Promise<string> {
    return this.post(`/setlanguage`, info);
  }

  roster(className: string): Promise<EnrollStudent[]> {
    return this.get(`/roster/${className}`);
  }

  async get(path: string, config?: AxiosRequestConfig): Promise<any> {
    await delay();
    const response = await axios.get(`${API_URL}${path}`, config);
    return response.data;
  }

  async post(
    path: string,
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<any> {
    const response = await axios.post(`${API_URL}${path}`, data, config);
    return response.data;
  }

  async put(
    path: string,
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<any> {
    await delay();
    const response = await axios.put(`${API_URL}${path}`, data, config);
    return response.data;
  }
}

export default new ApiService();
