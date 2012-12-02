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
package org.apache.myfaces.extensions.validator.core.storage;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.core.validation.message.FacesMessageHolder;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
 * @since x.x.3
 */
@ToDo(value = Priority.LOW, description = "optional parameter to deactivate sorting")
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultFacesMessageStorage implements FacesMessageStorage
{
    Map<String, ValidationResult> results = new HashMap<String, ValidationResult>();

    public void addFacesMessage(String clientId, FacesMessage facesMessage)
    {
        if(clientId == null)
        {
            clientId = "*";
        }

        if(!this.results.containsKey(clientId))
        {
            this.results.put(clientId, new ValidationResult());
        }

        this.results.get(clientId).addFacesMessageHolder(new FacesMessageHolder(facesMessage, clientId));
    }

    public List<FacesMessageHolder> getFacesMessages()
    {
        List<FacesMessageHolder> result = new ArrayList<FacesMessageHolder>();

        for(ValidationResult validationResult : this.results.values())
        {
            result.addAll(validationResult.getFacesMessageHolderList());
        }
        return result;
    }

    public void addAll()
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        List<FacesMessageHolder> facesMessageHolderList;

        for(ValidationResult validationResult : this.results.values())
        {
            facesMessageHolderList = validationResult.getFacesMessageHolderList();
            sortFacesMessageHolderList(facesMessageHolderList);

            for(FacesMessageHolder facesMessageHolder : facesMessageHolderList)
            {
                facesContext.addMessage(facesMessageHolder.getClientId(), facesMessageHolder.getFacesMessage());
            }
        }
    }

    private void sortFacesMessageHolderList(List<FacesMessageHolder> facesMessageHolderList)
    {
        Collections.sort(facesMessageHolderList, getFacesMessageComparator());
    }

    protected Comparator<FacesMessageHolder> getFacesMessageComparator()
    {
        return new Comparator<FacesMessageHolder>() {
            public int compare(FacesMessageHolder holder1, FacesMessageHolder holder2)
            {
                if(holder1.getFacesMessage().getSeverity() == null)
                {
                    return 1;
                }
                if(isSameSeverity(holder1, holder2))
                {
                    return compareMessageText(holder1.getFacesMessage(), holder2.getFacesMessage());
                }

                if(holder1.getFacesMessage().getSeverity().getOrdinal() >
                        holder2.getFacesMessage().getSeverity().getOrdinal())
                {
                    return -1;
                }
                else
                {
                    return 1;
                }
            }

            private int compareMessageText(FacesMessage facesMessage1, FacesMessage facesMessage2)
            {
                String text1 = facesMessage1.getDetail();
                String text2 = facesMessage2.getDetail();

                if(text1 == null)
                {
                    text1 = facesMessage1.getSummary();
                }

                if(text2 == null)
                {
                    text2 = facesMessage2.getSummary();
                }

                if(text1 == null)
                {
                    return 1;
                }

                if(text2 == null)
                {
                    return -1;
                }

                return text1.compareToIgnoreCase(text2);
            }
        };
    }

    private boolean isSameSeverity(FacesMessageHolder holder1, FacesMessageHolder holder2)
    {
        return holder1.getFacesMessage().getSeverity().equals(holder2.getFacesMessage().getSeverity());
    }
}
