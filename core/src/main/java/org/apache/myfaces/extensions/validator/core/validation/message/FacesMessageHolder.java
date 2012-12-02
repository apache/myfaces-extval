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
package org.apache.myfaces.extensions.validator.core.validation.message;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.application.FacesMessage;

/**
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class FacesMessageHolder
{
    private FacesMessage facesMessage;
    private String clientId;

    public FacesMessageHolder(FacesMessage facesMessage)
    {
        this.facesMessage = facesMessage;
    }

    public FacesMessageHolder(FacesMessage facesMessage, String clientId)
    {
        this.facesMessage = facesMessage;
        setClientId(clientId);
    }

    public FacesMessage getFacesMessage()
    {
        return facesMessage;
    }

    public String getClientId()
    {
        return clientId;
    }

    public void setClientId(String clientId)
    {
        if(!"*".equals(clientId))
        {
            this.clientId = clientId;
        }
    }
}
