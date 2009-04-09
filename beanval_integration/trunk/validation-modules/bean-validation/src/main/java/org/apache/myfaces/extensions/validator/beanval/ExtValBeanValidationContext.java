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
import org.apache.myfaces.extensions.validator.beanval.validation.message.interpolator.DefaultMessageInterpolator;
import org.apache.myfaces.extensions.validator.beanval.validation.message.interpolator.ExtValMessageInterpolatorAdapter;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import javax.validation.groups.Default;
import javax.validation.MessageInterpolator;
import javax.validation.Validation;
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
    protected final Log logger = LogFactory.getLog(getClass());

    private static final String KEY = ExtValBeanValidationContext.class.getName() + ":KEY";

    private static MessageInterpolator defaultMessageInterpolator = new DefaultMessageInterpolator(
            Validation.buildDefaultValidatorFactory().getMessageInterpolator());

    private static MessageResolver messageResolver;

    @ToDo(value = Priority.HIGH, description = "refactor to a pluggable GroupStorage")
    private Map<String, List<Class>> addedGroups = new HashMap<String, List<Class>>();

    @ToDo(value = Priority.HIGH, description = "refactor to a pluggable GroupStorage")
    private Map<String, List<Class>> restrictedGroups = new HashMap<String, List<Class>>();

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
        addGroupToGroupStorage(groupClass, viewId, componentId, this.addedGroups);
    }

    public void restrictGroup(Class groupClass)
    {
        restrictGroup(groupClass, FacesContext.getCurrentInstance().getViewRoot().getViewId());
    }

    public void restrictGroup(Class groupClass, String viewId)
    {
        restrictGroup(groupClass, viewId, null);
    }

    public void restrictGroup(Class groupClass, String viewId, String componentId)
    {
        addGroupToGroupStorage(groupClass, viewId, componentId, this.restrictedGroups);
    }

    private void addGroupToGroupStorage(Class groupClass, String viewId, String componentId,
                                        Map<String, List<Class>> groupStorage)
    {
        List<Class> groupList = groupStorage.get(getGroupKey(viewId, componentId));

        if(groupList == null)
        {
            groupList = new ArrayList<Class>();
            groupStorage.put(getGroupKey(viewId, componentId), groupList);
        }

        if(!groupList.contains(groupClass))
        {
            groupList.add(groupClass);
        }
    }

    public void resetGroup(String viewId)
    {
        resetGroups(viewId, null);
    }

    public void resetGroups(String viewId, String componentId)
    {
        this.addedGroups.put(getGroupKey(viewId, componentId), new ArrayList<Class>());
    }

    public Class[] getGroups()
    {
        if(this.addedGroups.size() < 1)
        {
            return new Class[] {Default.class};
        }

        List<Class> fullGroupList = new ArrayList<Class>();

        for(Map.Entry<String, List<Class>> currentGroupEntry : this.addedGroups.entrySet())
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
        if(this.addedGroups.size() < 1)
        {
            if(!"true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_IMPLICIT_DEFAULT_GROUP_VALIDATION))
            {
                return new Class[] {Default.class};
            }
            return new Class[0];
        }

        String key = getGroupKey(viewId, null);
        List<Class> resultListForPage = buildResultFor(key, this.addedGroups);

        key = getGroupKey(viewId, componentId);
        List<Class> resultListForComponent = buildResultFor(key, this.addedGroups);

        //remove restricted groups
        Class[] resultsForPage = filterResult(getGroupKey(viewId, null), resultListForPage);
        Class[] resultsForComponent = filterResult(getGroupKey(viewId, componentId), resultListForComponent);

        if(resultsForPage.length == 0)
        {
            if(resultsForComponent.length == 0)
            {
                if(this.logger.isDebugEnabled())
                {
                    this.logger.debug("no groups for group-validation available." +
                            "maybe you restricted all groups or you aren't using groups." +
                            "bean validation will use the default group for validation");
                }
            }
            return resultsForComponent;
        }
        else if(resultsForComponent.length == 0)
        {
            return resultsForPage;
        }

        return mergeResults(resultsForPage, resultsForComponent);
    }

    private List<Class> buildResultFor(String key, Map<String, List<Class>> groupStorage)
    {
        List<Class> list = groupStorage.get(key);
        return (list != null) ? list : new ArrayList<Class>();
    }

    private Class[] filterResult(String key, List<Class> addedGroups)
    {
        List<Class> restrictedGroups = buildResultFor(key, this.restrictedGroups);
        List<Class> results = new ArrayList<Class>();

        for(Class currentGroup : addedGroups)
        {
            if(!restrictedGroups.contains(currentGroup))
            {
                results.add(currentGroup);
            }
        }

        return results.toArray(new Class[results.size()]);
    }

    private Class[] mergeResults(Class[] resultsForPage, Class[] resultsForComponent)
    {
        Class[] mergedResult = new Class[resultsForPage.length + resultsForComponent.length];

        System.arraycopy(resultsForPage, 0, mergedResult, 0, resultsForPage.length);
        System.arraycopy(resultsForComponent, 0, mergedResult, resultsForPage.length, resultsForComponent.length);

        return mergedResult;
    }

    public void removeGroup(Class groupClass)
    {
        removeGroup(groupClass, FacesContext.getCurrentInstance().getViewRoot().getViewId());
    }

    public void removeGroup(Class groupClass, String viewId)
    {
        removeGroup(groupClass, viewId, null);
    }

    @ToDo(Priority.HIGH)
    public void removeGroup(Class groupClass, String viewId, String componentId)
    {
        this.addedGroups.remove(getGroupKey(viewId, componentId));
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

    public MessageInterpolator getMessageInterpolator()
    {
        if(messageResolver != null)
        {
            return new ExtValMessageInterpolatorAdapter(defaultMessageInterpolator, messageResolver);
        }

        return defaultMessageInterpolator;
    }

    public static void setMessageResolver(MessageResolver customMessageResolver)
    {
        messageResolver = customMessageResolver;
    }
}
