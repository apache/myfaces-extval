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
package org.apache.myfaces.extensions.validator.beanval.storage;

import org.apache.myfaces.extensions.validator.util.GroupUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;

import javax.faces.context.FacesContext;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;

/**
 * storage implementation for model-validation entries
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultModelValidationStorage implements ModelValidationStorage
{
    private Map<String, List<ModelValidationEntry>> modelValidationEntries =
            new HashMap<String, List<ModelValidationEntry>>();

    private List<String> componentsOfRequest = new ArrayList<String>();

    public void addModelValidationEntry(ModelValidationEntry modelValidationEntry)
    {
        String clientId = getCurrentClientId(modelValidationEntry);

        List<ModelValidationEntry> modelValidationEntryList = resolveModelValidationEntryList(
                modelValidationEntry, clientId);

        addModelValidationEntry(modelValidationEntryList, modelValidationEntry);
    }

    private String getCurrentClientId(ModelValidationEntry modelValidationEntry)
    {
        String clientId = null;

        if(modelValidationEntry.getComponent() != null)
        {
            clientId = modelValidationEntry.getComponent().getClientId(FacesContext.getCurrentInstance());

            if(!this.componentsOfRequest.contains(clientId))
            {
                this.componentsOfRequest.add(clientId);
            }
        }
        return clientId;
    }

    private List<ModelValidationEntry> resolveModelValidationEntryList(
            ModelValidationEntry modelValidationEntry, String clientId)
    {
        List<ModelValidationEntry> modelValidationEntryList =
                this.modelValidationEntries.get(GroupUtils.getGroupKey(
                        modelValidationEntry.getViewId(), clientId));

        if(modelValidationEntryList == null)
        {
            modelValidationEntryList = new ArrayList<ModelValidationEntry>();
            this.modelValidationEntries.put(GroupUtils.getGroupKey(
                    modelValidationEntry.getViewId(), clientId), modelValidationEntryList);
        }
        return modelValidationEntryList;
    }

    private void addModelValidationEntry(
            List<ModelValidationEntry> modelValidationEntryList, ModelValidationEntry modelValidationEntry)
    {
        if(!modelValidationEntryList.contains(modelValidationEntry))
        {
            modelValidationEntryList.add(modelValidationEntry);
        }
    }

    public List<ModelValidationEntry> getModelValidationEntriesToValidate()
    {
        String viewId = FacesContext.getCurrentInstance().getViewRoot().getViewId();
        List<ModelValidationEntry> result = new ArrayList<ModelValidationEntry>();

        addEntriesForComponents(viewId, result);

        addEntriesForPage(viewId, result);

        return result;
    }

    private void addEntriesForComponents(String viewId, List<ModelValidationEntry> result)
    {
        for(String currentClientId : this.componentsOfRequest)
        {
            result.addAll(getModelValidationEntries(viewId, currentClientId));
        }
    }

    private void addEntriesForPage(String viewId, List<ModelValidationEntry> result)
    {
        result.addAll(getModelValidationEntries(viewId));
    }

    private List<ModelValidationEntry> buildModelValidationEntryList(
            String key, Map<String, List<ModelValidationEntry>> groupStorage)
    {
        List<ModelValidationEntry> list;

        if(key != null && key.endsWith("*"))
        {
            list = new ArrayList<ModelValidationEntry>();
            for(Map.Entry<String,List<ModelValidationEntry>> entry : groupStorage.entrySet())
            {
                if(entry.getKey().substring(0, entry.getKey().indexOf("@"))
                        .equals(key.substring(0, key.indexOf("@"))))
                {
                    list.addAll(entry.getValue());
                }
            }
            return list;
        }

        list = groupStorage.get(key);
        return (list != null) ? list : new ArrayList<ModelValidationEntry>();
    }

    private List<ModelValidationEntry> getModelValidationEntries(String viewId)
    {
        return getModelValidationEntries(viewId, null);
    }

    private List<ModelValidationEntry> getModelValidationEntries(String viewId, String clientId)
    {
        if(this.modelValidationEntries.size() < 1)
        {
            return new ArrayList<ModelValidationEntry>();
        }

        //add found groups
        String key;
        List<ModelValidationEntry> resultListForPage = null;

        if(!"*".equals(clientId))
        {
            key = GroupUtils.getGroupKey(viewId, null);
            resultListForPage =
                    buildModelValidationEntryList(key, this.modelValidationEntries);
        }

        key = GroupUtils.getGroupKey(viewId, clientId);
        List<ModelValidationEntry> resultListForComponent =
                buildModelValidationEntryList(key, this.modelValidationEntries);

        if(resultListForPage == null || resultListForPage.isEmpty())
        {
            return resultListForComponent;
        }
        else if(resultListForComponent.isEmpty())
        {
            return resultListForPage;
        }

        return mergeResults(resultListForPage, resultListForComponent);
    }

    private List<ModelValidationEntry> mergeResults(
            List<ModelValidationEntry> resultListForPage, List<ModelValidationEntry> resultListForComponent)
    {
        List<ModelValidationEntry> mergedResult = new ArrayList<ModelValidationEntry>();
        mergedResult.addAll(resultListForPage);
        mergedResult.addAll(resultListForComponent);
        return mergedResult;
    }
}
