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

import org.apache.myfaces.extensions.validator.beanval.validation.message.interpolator.DefaultMessageInterpolator;
import org.apache.myfaces.extensions.validator.beanval.validation.message.interpolator.ExtValMessageInterpolatorAdapter;
import org.apache.myfaces.extensions.validator.beanval.validation.strategy.BeanValidationVirtualValidationStrategy;
import org.apache.myfaces.extensions.validator.beanval.storage.ModelValidationEntry;
import org.apache.myfaces.extensions.validator.beanval.storage.ModelValidationStorage;
import org.apache.myfaces.extensions.validator.beanval.annotation.BeanValidation;
import org.apache.myfaces.extensions.validator.beanval.annotation.ModelValidation;
import org.apache.myfaces.extensions.validator.beanval.util.BeanValidationUtils;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.storage.GroupStorage;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import javax.validation.MessageInterpolator;
import javax.validation.ValidatorFactory;
import java.util.Map;
import java.util.List;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public class ExtValBeanValidationContext implements GroupStorage, ModelValidationStorage
{
    protected final Log logger = LogFactory.getLog(getClass());

    private static final String KEY = ExtValBeanValidationContext.class.getName() + ":KEY";

    private MessageInterpolator defaultMessageInterpolator;

    private MessageResolver messageResolver;

    private GroupStorage groupStorage;

    private ModelValidationStorage modelValidationStorage;

    private ExtValBeanValidationContext()
    {
        initGroupStorage();
        initModelValidationStorage();

        initMessageResolver();
        initMessageInterpolator();
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

    public ValidatorFactory getValidatorFactory()
    {
        Object validatorFactory = ExtValContext.getContext().getGlobalProperty(ValidatorFactory.class.getName());

        if(validatorFactory instanceof ValidatorFactory)
        {
            return (ValidatorFactory)validatorFactory;
        }

        if(this.logger.isWarnEnabled())
        {
            this.logger.warn("fallback to the default bv validator factory");
        }
        return BeanValidationUtils.getDefaultValidatorFactory();
    }

    public MessageInterpolator getMessageInterpolator()
    {
        if(this.messageResolver != null)
        {
            return new ExtValMessageInterpolatorAdapter(this.defaultMessageInterpolator, this.messageResolver);
        }

        return this.defaultMessageInterpolator;
    }

    public void addGroup(Class groupClass, String viewId, String clientId)
    {
        this.groupStorage.addGroup(groupClass, viewId, clientId);
    }

    public void restrictGroup(Class groupClass, String viewId, String clientId)
    {
        this.groupStorage.restrictGroup(groupClass, viewId, clientId);
    }

    public Class[] getGroups(String viewId, String clientId)
    {
        return this.groupStorage.getGroups(viewId, clientId);
    }

    public void addModelValidationEntry(ModelValidationEntry modelValidationEntry)
    {
        this.modelValidationStorage.addModelValidationEntry(modelValidationEntry);
    }

    public List<ModelValidationEntry> getModelValidationEntriesToValidate()
    {
        return this.modelValidationStorage.getModelValidationEntriesToValidate();
    }

    private void initGroupStorage()
    {
        this.groupStorage = ExtValUtils
                .getStorage(GroupStorage.class, BeanValidation.class.getName());
    }

    private void initModelValidationStorage()
    {
        this.modelValidationStorage = ExtValUtils.
                getStorage(ModelValidationStorage.class, ModelValidation.class.getName());
    }

    @ToDo(Priority.HIGH)
    private void initMessageInterpolator()
    {
        Object foundBean = ExtValUtils.getELHelper().getBean(MessageInterpolator.class.getName().replace(".", "_"));

        if(foundBean instanceof MessageInterpolator)
        {
            this.defaultMessageInterpolator = (MessageInterpolator)foundBean;
        }
        else
        {
            this.defaultMessageInterpolator = new DefaultMessageInterpolator(
                BeanValidationUtils.getDefaultValidatorFactory().getMessageInterpolator());
        }
    }

    private void initMessageResolver()
    {
        this.messageResolver = ExtValUtils.getMessageResolverForValidationStrategy(getBeanValidationStrategy());
    }

    private ValidationStrategy getBeanValidationStrategy()
    {
        return new BeanValidationVirtualValidationStrategy(null, null);
    }
}
