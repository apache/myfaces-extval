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
import org.apache.myfaces.extensions.validator.core.JsfProjectStage;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;
import javax.validation.MessageInterpolator;
import javax.validation.ValidatorFactory;
import java.util.Map;
import java.util.List;
import java.util.logging.Logger;

/**
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public class ExtValBeanValidationContext implements GroupStorage, ModelValidationStorage
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private static final String KEY = ExtValBeanValidationContext.class.getName() + ":KEY";

    protected ValidatorFactory validatorFactory;

    protected MessageInterpolator defaultMessageInterpolator;

    protected MessageResolver messageResolver;

    protected GroupStorage groupStorage;

    protected ModelValidationStorage modelValidationStorage;

    protected boolean developmentMode = false;

    protected ExtValBeanValidationContext()
    {
        initGroupStorage();
        initModelValidationStorage();

        initMessageResolver();
        initMessageInterpolator();

        if (JsfProjectStage.is(JsfProjectStage.Development))
        {
            this.developmentMode = true;
        }
    }

    @SuppressWarnings({"unchecked"})
    public static ExtValBeanValidationContext getCurrentInstance()
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();

        Map requestMap = facesContext.getExternalContext().getRequestMap();

        ExtValBeanValidationContext currentContext = (ExtValBeanValidationContext)requestMap.get(KEY);

        if(currentContext == null)
        {
            currentContext = ExtValBeanValidationModuleConfiguration.get().customExtValBeanValidationContext();

            if(currentContext == null)
            {
                currentContext = new ExtValBeanValidationContext();
            }
            requestMap.put(KEY, currentContext);
        }

        return currentContext;
    }

    public ValidatorFactory getValidatorFactory()
    {
        if(this.validatorFactory != null)
        {
            return this.validatorFactory;
        }

        this.validatorFactory = ExtValBeanValidationModuleConfiguration.get().customValidatorFactory();

        if(this.validatorFactory == null)
        {
            if(this.developmentMode)
            {
                this.logger.warning("fallback to the default bv validator factory");
            }
            this.validatorFactory = BeanValidationUtils.getDefaultValidatorFactory();
        }

        return this.validatorFactory;
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

    protected void initGroupStorage()
    {
        this.groupStorage = ExtValUtils
                .getStorage(GroupStorage.class, BeanValidation.class.getName());
    }

    protected void initModelValidationStorage()
    {
        this.modelValidationStorage = ExtValUtils.
                getStorage(ModelValidationStorage.class, ModelValidation.class.getName());
    }

    protected void initMessageInterpolator()
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

    protected void initMessageResolver()
    {
        this.messageResolver = ExtValUtils.getMessageResolverForValidationStrategy(getBeanValidationStrategy());
    }

    private ValidationStrategy getBeanValidationStrategy()
    {
        return new BeanValidationVirtualValidationStrategy(null, null);
    }
}
