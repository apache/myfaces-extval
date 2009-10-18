/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.demo.domain;

import org.apache.myfaces.extensions.validator.beanval.payload.DisableClientSideValidation;

import javax.validation.constraints.Size;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Past;
import java.util.Date;

public class Person
{
    @Size(min = 2, max = 20)
    private String firstName;

    @NotNull(payload = DisableClientSideValidation.class)
    @Size(min = 2, max = 20, payload = DisableClientSideValidation.class)
    private String lastName;

    @Size(min = 2, max = 10)
    private String password;

    @Past
    @NotNull
    private Date birthday;

    @Past
    private Date finalExam;

    @NotNull
    private String nickName;

    @Size(min = 6, max = 60)
    private String email;

    @NotNull
    private int numberOfSiblings;

    @Size(min = 2, max = 10)
    private String role = "user";

    public String getFirstName()
    {
        return firstName;
    }

    public void setFirstName(String firstName)
    {
        this.firstName = firstName;
    }

    public String getLastName()
    {
        return lastName;
    }

    public void setLastName(String lastName)
    {
        this.lastName = lastName;
    }

    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    public String getRole()
    {
        return role;
    }

    public void setRole(String role)
    {
        this.role = role;
    }

    public Date getBirthday()
    {
        return birthday;
    }

    public void setBirthday(Date birthday)
    {
        this.birthday = birthday;
    }

    public Date getFinalExam()
    {
        return finalExam;
    }

    public void setFinalExam(Date finalExam)
    {
        this.finalExam = finalExam;
    }

    public String getNickName()
    {
        return nickName;
    }

    public void setNickName(String nickName)
    {
        this.nickName = nickName;
    }

    public String getEmail()
    {
        return email;
    }

    public void setEmail(String email)
    {
        this.email = email;
    }

    public int getNumberOfSiblings()
    {
        return numberOfSiblings;
    }

    public void setNumberOfSiblings(int numberOfSiblings)
    {
        this.numberOfSiblings = numberOfSiblings;
    }
}
