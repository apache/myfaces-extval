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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;
import org.apache.myfaces.extensions.validator.beanval.annotation.ModelValidation;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class ModelValidationEntry
{
    private UIComponent component;
    private List<Class> groups = new ArrayList<Class>();
    private List<Object> validationTargets = new ArrayList<Object>();
    private boolean displayMessageInline = false;
    private String customMessage = ModelValidation.DEFAULT_MESSAGE;
    
    //the original source where the extval-bv meta-data has been found
    private Object metaDataSourceObject;
    private String viewId = FacesContext.getCurrentInstance().getViewRoot().getViewId();

    public void addGroup(Class group)
    {
        if(!this.groups.contains(group))
        {
            if(!(this.groups instanceof ArrayList))
            {
                List<Class> newGroupList = new ArrayList<Class>();

                for(Class currentClass : this.groups)
                {
                    newGroupList.add(currentClass);
                }
                this.groups = newGroupList;
            }

            this.groups.add(group);
        }
    }

    public void removeGroup(Class group)
    {
        this.groups.remove(group);
    }

    public void addValidationTarget(Object target)
    {
        if(!this.validationTargets.contains(target))
        {
            if(!(this.validationTargets instanceof ArrayList))
            {
                List<Object> validationTargetList = new ArrayList<Object>();

                for(Object currentTarget : this.validationTargets)
                {
                    validationTargetList.add(currentTarget);
                }
                this.validationTargets = validationTargetList;
            }

            this.validationTargets.add(target);
        }
    }

    /*
     * generated
     */
    public UIComponent getComponent()
    {
        return component;
    }

    public void setComponent(UIComponent component)
    {
        this.component = component;
    }

    public Class[] getGroups()
    {
        return this.groups.toArray(new Class[this.groups.size()]);
    }

    public void setGroups(List<Class> groups)
    {
        this.groups = groups;
    }

    public List<Object> getValidationTargets()
    {
        return validationTargets;
    }

    public Object getMetaDataSourceObject()
    {
        return metaDataSourceObject;
    }

    public void setMetaDataSourceObject(Object metaDataSourceObject)
    {
        this.metaDataSourceObject = metaDataSourceObject;
    }

    @SuppressWarnings({"RedundantIfStatement"})
    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        ModelValidationEntry that = (ModelValidationEntry) o;

        if (component != null ? !component.equals(that.component) : that.component != null)
        {
            return false;
        }
        if (!groups.equals(that.groups))
        {
            return false;
        }
        if (!validationTargets.equals(that.validationTargets))
        {
            return false;
        }

        return true;
    }

    public boolean isDisplayMessageInline()
    {
        return displayMessageInline;
    }

    public void setDisplayMessageInline(boolean displayMessageInline)
    {
        this.displayMessageInline = displayMessageInline;
    }

    public String getCustomMessage()
    {
        return customMessage;
    }

    public void setCustomMessage(String customMessage)
    {
        this.customMessage = customMessage;
    }

    @Override
    public int hashCode()
    {
        int result = component != null ? component.hashCode() : 0;
        result = 31 * result + groups.hashCode();
        result = 31 * result + validationTargets.hashCode();
        return result;
    }

    public String getViewId()
    {
        return viewId;
    }

    public void setViewId(String viewId)
    {
        this.viewId = viewId;
    }
}
