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
package org.apache.myfaces.extensions.validator.core.startup;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.HtmlCoreComponentsValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.ViolationSeverityValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.FacesMessagePropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .BeanValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .SimpleValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .DefaultValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .CustomConventionValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .CustomConfiguredValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
        .CustomConventionAnnotationToValidationStrategyNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
        .DefaultAnnotationToValidationStrategyNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
        .CustomConfiguredAnnotationToValidationStrategyNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
        .SimpleAnnotationToValidationStrategyNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
        .AnnotationToValidationStrategyBeanNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.mapper
        .CustomConfiguredValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.mapper
        .CustomConventionValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.mapper
        .DefaultValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.mapper
        .DefaultModuleValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.mapper
        .SimpleValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultViolationSeverityInterpreter;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererProxy;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.ExtValInformation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValStartupListener extends AbstractStartupListener
{
    private static final long serialVersionUID = -2504826421086572012L;

    protected void init()
    {
        if(logger.isInfoEnabled())
        {
            if(ExtValInformation.VERSION != null)
            {
                logger.info("starting up MyFaces Extensions Validator v" + ExtValInformation.VERSION);
            }
            else
            {
                logger.info("starting up MyFaces Extensions Validator");
            }
        }

        ExtValContext.getContext().registerRendererInterceptor(new ValidationInterceptor());

        ExtValContext.getContext()
                .addGlobalProperty(ExtValRendererProxy.KEY, ExtValRendererProxy.class.getName(), false);

        initNameMappers();
        initValidationExceptionInterceptors();
        initViolationSeverityInterceptors();
        initPropertyValidationInterceptors();
        executeCustomStartupListener();
    }

    private void initNameMappers()
    {
        String deactivateDefaultNameMappers = WebXmlParameter.DEACTIVATE_DEFAULT_NAME_MAPPERS;
        if ((deactivateDefaultNameMappers != null && deactivateDefaultNameMappers.equalsIgnoreCase("true")))
        {
            return;
        }

        //register metadata to validation strategy name mapper
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new CustomConfiguredAnnotationToValidationStrategyNameMapper());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new CustomConventionAnnotationToValidationStrategyNameMapper());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new DefaultAnnotationToValidationStrategyNameMapper());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new SimpleAnnotationToValidationStrategyNameMapper());

        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new AnnotationToValidationStrategyBeanNameMapper(
                        new CustomConfiguredAnnotationToValidationStrategyNameMapper()));
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new AnnotationToValidationStrategyBeanNameMapper(
                        new CustomConventionAnnotationToValidationStrategyNameMapper()));
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new AnnotationToValidationStrategyBeanNameMapper(
                        new DefaultAnnotationToValidationStrategyNameMapper()));
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new AnnotationToValidationStrategyBeanNameMapper(
                        new SimpleAnnotationToValidationStrategyNameMapper()));

        //register validation strategy to message resolver name mapper
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new CustomConfiguredValidationStrategyToMsgResolverNameMapper());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new CustomConventionValidationStrategyToMsgResolverNameMapper());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new DefaultValidationStrategyToMsgResolverNameMapper());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new DefaultModuleValidationStrategyToMsgResolverNameMapper());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new SimpleValidationStrategyToMsgResolverNameMapper());

        //register validation strategy to metadata transformer name mapper
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new CustomConfiguredValidationStrategyToMetaDataTransformerNameMapper());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new CustomConventionValidationStrategyToMetaDataTransformerNameMapper());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new DefaultValidationStrategyToMetaDataTransformerNameMapper());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new SimpleValidationStrategyToMetaDataTransformerNameMapper());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new BeanValidationStrategyToMetaDataTransformerNameMapper());
    }

    private void executeCustomStartupListener()
    {
        String customStartupListenerName = ExtValContext.getContext().getInformationProviderBean()
            .get(CustomInformation.STARTUP_LISTENER);
        AbstractStartupListener customStartupListener =
            (AbstractStartupListener)ClassUtils.tryToInstantiateClassForName(customStartupListenerName);

        if(customStartupListener != null)
        {
            if(logger.isInfoEnabled())
            {
                logger.info("start init of " + customStartupListener.getClass().getName());
            }

            customStartupListener.init();

            if(logger.isInfoEnabled())
            {
                logger.info("init of " + customStartupListener.getClass().getName() + " finished");
            }
        }
    }

    private void initValidationExceptionInterceptors()
    {
        ExtValContext.getContext().addValidationExceptionInterceptor(
                new HtmlCoreComponentsValidationExceptionInterceptor());
        ExtValContext.getContext().addValidationExceptionInterceptor(
                new ViolationSeverityValidationExceptionInterceptor());
    }

    private void initViolationSeverityInterceptors()
    {
        ExtValContext.getContext().setViolationSeverityInterpreter(new DefaultViolationSeverityInterpreter(), false);
    }

    private void initPropertyValidationInterceptors()
    {
        ExtValContext.getContext().addPropertyValidationInterceptor(new FacesMessagePropertyValidationInterceptor());
    }
}
