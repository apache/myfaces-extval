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
package org.apache.myfaces.extensions.validator.beanval;

import org.apache.myfaces.extensions.validator.beanval.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;

import javax.faces.context.FacesContext;
import javax.validation.groups.Default;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 * @since 1.x.3
 */
public class ExtValBeanValidationContext
{
    private static final String KEY = ExtValBeanValidationContext.class.getName() + ":KEY";

    @ToDo(value = Priority.HIGH, description = "refactor to a pluggable GroupStorage")
    private Map<String, List<Class>> currentGroups = new HashMap<String, List<Class>>();

    @ToDo(value = Priority.HIGH,
            description = "idea: use it for impl. group support in a more extensible way - target: move it to the core")
    private List<PropertyValidationInterceptor> propertyValidationInterceptors
            = new ArrayList<PropertyValidationInterceptor>();

    private ExtValBeanValidationContext()
    {
    }

    @SuppressWarnings({"unchecked"})
    public static ExtValBeanValidationContext getCurrentInstance()
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();

        Map requestMap = facesContext.getExternalContext().getRequestMap();

        ExtValBeanValidationContext currentContext = (ExtValBeanValidationContext)requestMap.get(KEY);

        if(currentContext == null)
        {
            currentContext = new ExtValBeanValidationContext();
            requestMap.put(KEY, currentContext);
        }

        return currentContext;
    }

    private String getGroupKey(String viewId, String componentId)
    {
        return componentId == null ? viewId : viewId + ":" + componentId;
    }

    public void addGroup(Class groupClass)
    {
        addGroup(groupClass, FacesContext.getCurrentInstance().getViewRoot().getViewId());
    }

    public void addGroup(Class groupClass, String viewId)
    {
        addGroup(groupClass, viewId, null);
    }

    public void addGroup(Class groupClass, String viewId, String componentId)
    {
        List<Class> groupList = this.currentGroups.get(getGroupKey(viewId, componentId));
        if(groupList == null)
        {
            groupList = new ArrayList<Class>();
            this.currentGroups.put(getGroupKey(viewId, componentId), groupList);
        }
        groupList.add(groupClass);
    }

    public Class[] getGroups()
    {
        if(this.currentGroups.size() < 1)
        {
            return new Class[] {Default.class};
        }

        List<Class> fullGroupList = new ArrayList<Class>();

        for(Map.Entry<String, List<Class>> currentGroupEntry : this.currentGroups.entrySet())
        {
            fullGroupList.addAll(currentGroupEntry.getValue());

        }
        return (Class[]) fullGroupList.toArray();
    }

    public Class[] getGroups(String viewId)
    {
        return getGroups(viewId, null);
    }

    public Class[] getGroups(String viewId, String componentId)
    {
        if(this.currentGroups.size() < 1)
        {
            return new Class[] {Default.class};
        }

        String key = getGroupKey(viewId, componentId);

        return (Class[]) this.currentGroups.get(key).toArray();
    }

    public void removeGroup(Class groupClass)
    {
        removeGroup(groupClass, FacesContext.getCurrentInstance().getViewRoot().getViewId());
    }

    public void removeGroup(Class groupClass, String viewId)
    {
        removeGroup(groupClass, viewId, null);
    }

    public void removeGroup(Class groupClass, String viewId, String componentId)
    {
        this.currentGroups.remove(getGroupKey(viewId, componentId));
    }

    @ToDo(Priority.HIGH)
    public void registerPropertyValidationInterceptor(PropertyValidationInterceptor propertyValidationInterceptor)
    {
    }

    @ToDo(Priority.HIGH)
    public void denyPropertyValidationInterceptor(Class<? extends PropertyValidationInterceptor> groupInterceptorClass)
    {
    }

    public List<PropertyValidationInterceptor> getPropertyValidationInterceptors()
    {
        return this.propertyValidationInterceptors;
    }
}
